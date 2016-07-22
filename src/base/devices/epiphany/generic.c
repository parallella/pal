#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdbool.h>

#include "config.h"
#include "dev_epiphany.h"
#include "ctrl.h"
#include "pal_base.h"
#include <common.h>
#include "../../pal_base_private.h"
#include "loader.h"
#include "generic.h"

int epiphany_dev_early_init(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    dev_data->initialized = true;
}

void epiphany_dev_late_fini(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    dev_data->initialized = false;
}

p_dev_t epiphany_dev_init(struct dev *dev, int flags)
{
    int err;
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    if (!epiphany->initialized)
        return p_ref_err(ENODEV);

    /* Be idempotent if already initialized. It might be a better idea to
     * return EBUSY instead */
    if (epiphany->opened)
        return dev;

    epiphany->ctrl = (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;

#if 0
    /* I don't think this is needed here, soft reset on load should be enough */
    err = epiphany_reset_system(epiphany);
    if (err)
        return p_ref_err(-err);
#endif

    /* Clear control structure */
    memset(epiphany->ctrl, 0 , sizeof(*epiphany->ctrl));

    epiphany->opened = true;

    return dev;
}

void epiphany_dev_fini(struct dev *dev)
{
    struct epiphany_dev *data = to_epiphany_dev(dev);

    if (data->opened) {
        data->opened = false;
    }
}

int epiphany_dev_query(struct dev *dev, int property)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    switch (property) {
    case P_PROP_TYPE:
        return P_DEV_EPIPHANY;
    case P_PROP_NODES:
        return epiphany->rows * epiphany->cols;
    case P_PROP_TOPOLOGY:
        return 2;
    case P_PROP_ROWS:
        return epiphany->rows;
    case P_PROP_COLS:
        return epiphany->cols;
    case P_PROP_ROWBASE:
        return epiphany->row_base;
    case P_PROP_COLBASE:
        return epiphany->col_base;
    case P_PROP_PLANES:
        return 1;
    case P_PROP_PLANEBASE:
        return 0;
    case P_PROP_CHIPROWS:
        return epiphany->rows;
    case P_PROP_CHIPCOLS:
        return epiphany->cols;
    case P_PROP_SIMD:
        return 1;
    case P_PROP_MEMSIZE:
        return epiphany->sram_size;
    case P_PROP_MEMBASE:
        return (epiphany->row_base << 6 | epiphany->col_base) << 20;
    case P_PROP_VERSION:
        return 0xdeadbeef;
    case P_PROP_MEMARCH:
    case P_PROP_WHOAMI:
        return -ENOSYS;
    }
    return -EINVAL;
}


struct team *epiphany_dev_open(struct team *team)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    if (team->start < 0 || team->count < 0 || 16 < team->start + team->count)
        return p_ref_err(EINVAL);

    /* Open was done in init */
    if (!epiphany->opened)
        return p_ref_err(EBADF);

    return team;
}

int epiphany_dev_run(struct dev *dev, struct team *team, struct prog *prog,
                     const char *function, int start, int count, int argn,
                     const p_arg_t *args, int flags)
{
    int err;
    int i;
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    if (start < 0 || count <= 0)
        return -EINVAL;

    if (team->count < start + count)
        return -EINVAL;

    if (!epiphany->opened)
        return -EBADF;

    err = epiphany_soft_reset(team, team->start + start, count);
    if (err) {
        /* WARN: soft reset failed */
        return err;
    }

    err = epiphany_load(team, prog, start, count, flags, argn, args, function);
    if (err)
        return err;

    /* Mark as scheduled */
    for (i = team->start + start; i < team->start + start + count; i++)
        epiphany->ctrl->status[i] = STATUS_SCHEDULED;

    /* Ideally a *system* (not host CPU-only) memory barrier here */
    __sync_synchronize();

    epiphany_start(team, team->start + start, count, flags);

    return 0;
}

int epiphany_dev_wait(struct dev *dev, struct team *team)
{
    unsigned i, j = 0;
    bool need_wait = true;
    struct epiphany_dev *data = to_epiphany_dev(dev);

    while (true) {
        need_wait = false;
        for (i = team->start; i < team->start + team->count; i++) {
            switch (data->ctrl->status[i]) {
            case STATUS_SCHEDULED:
                /* TODO: Time out if same proc is in scheduled state too long.
                 * If program does not start immediately something has gone
                 * wrong.
                 */
            case STATUS_RUNNING:
                need_wait = true;
                break;
            case STATUS_NONE:
            case STATUS_DONE:
            default:
                continue;
            }
        }
        if (!need_wait)
            break;

        /* Manual check to be compatible with non-pal device programs */
        if (!(++j % 100)) {
            if (epiphany_is_team_done(team))
                break;
        }

        /* Don't burn CPU. Need HW/Kernel support for blocking wait */
        usleep(1000);
    }

    return 0;
}
