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
#include "../../../pal_base_private.h"
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
        return epiphany->dev.size.row * epiphany->dev.size.col;
    case P_PROP_TOPOLOGY:
        return 2;
    case P_PROP_ROWS:
        return epiphany->dev.size.row;
    case P_PROP_COLS:
        return epiphany->dev.size.col;
    case P_PROP_ROWBASE:
        return epiphany->dev.start.row;
    case P_PROP_COLBASE:
        return epiphany->dev.start.col;
    case P_PROP_PLANES:
        return 1;
    case P_PROP_PLANEBASE:
        return 0;
    case P_PROP_CHIPROWS:
        return epiphany->dev.size.row;
    case P_PROP_CHIPCOLS:
        return epiphany->dev.size.col;
    case P_PROP_SIMD:
        return 1;
    case P_PROP_MEMSIZE:
        return epiphany->sram_size;
    case P_PROP_MEMBASE:
        return (epiphany->dev.start.row << 6 | epiphany->dev.start.col) << 20;
    case P_PROP_VERSION:
        return 0xdeadbeef;
    case P_PROP_MEMARCH:
    case P_PROP_WHOAMI:
        return -ENOSYS;
    }
    return -EINVAL;
}

int epiphany_team_coords_to_dev_coords(struct team *team,
                                       const p_coords_t *team_coords,
                                       p_coords_t *dev_coords)
{
    p_coords_t scratch_dev_coords;

    /* Allow NULL ptr argument */
    if (!dev_coords)
        dev_coords = &scratch_dev_coords;

    switch (team->topology) {
    case P_TOPOLOGY_FLAT:
        dev_coords->row =
            team->dev->start.row + team_coords->id / team->dev->size.col;
        dev_coords->col =
            team->dev->start.col + team_coords->id % team->dev->size.col;
        break;
    case P_TOPOLOGY_2D:
        dev_coords->row = team->dev->start.row + team_coords->row;
        dev_coords->col = team->dev->start.col + team_coords->col;
        break;
    default:
        return -EINVAL;
    }

    if (   team->dev->start.row + team->dev->size.row <= dev_coords->row
        || team->dev->start.col + team->dev->size.col <= dev_coords->col)
        return -EINVAL;

    return 0;
}

int epiphany_last_coords(struct team *team, p_coords_t *last_coords)
{
    switch (team->topology) {
    case P_TOPOLOGY_FLAT:
        last_coords->id = team->start.id + team->size.id - 1;
        break;
    case P_TOPOLOGY_2D:
        last_coords->row = team->start.row + team->size.row - 1;
        last_coords->col = team->start.col + team->size.col - 1;
        break;
    default:
        return -EINVAL;
    }

    return 0;
}

struct team *epiphany_dev_open(struct team *team)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);
    p_coords_t last_coords;

    /* Open was done in init */
    if (!epiphany->opened)
        return p_ref_err(EBADF);

    if (epiphany_team_coords_to_dev_coords(team, &team->start, NULL))
        return p_ref_err(EINVAL);

    if (epiphany_last_coords(team, &last_coords))
        return p_ref_err(EINVAL);

    if (epiphany_team_coords_to_dev_coords(team, &last_coords, NULL))
        return p_ref_err(EINVAL);

    return team;
}

static unsigned ctrl_offset(struct team *team, int rank)
{
    unsigned offset;
    p_coords_t team_coords, dev_coords;

    p_rank_to_coords(team, rank, &team_coords, 0);
    epiphany_team_coords_to_dev_coords(team, &team_coords, &dev_coords);

    offset = (dev_coords.row - team->dev->start.row) * team->dev->size.col +
             (dev_coords.col - team->dev->start.col);

    return offset;
}

int epiphany_dev_load(struct team *team, int start, int count,
                      struct prog *prog, const char *function, int argn,
                      const p_arg_t *args)
{
    int err;
    int i;
    p_coords_t start_coords, last_coords;
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    if (!epiphany->opened)
        return -EBADF;

    if (start < 0 || count <= 0)
        return -EINVAL;

    if (p_rank_to_coords(team, start, &start_coords, 0))
        return -EINVAL;

    if (p_rank_to_coords(team, start + count - 1, &last_coords, 0))
        return -EINVAL;

    if (epiphany_team_coords_to_dev_coords(team, &start_coords, NULL))
        return -EINVAL;

    if (epiphany_team_coords_to_dev_coords(team, &last_coords, NULL))
        return -EINVAL;

    err = epiphany_soft_reset(team, start, count);
    if (err) {
        /* WARN: soft reset failed */
        return err;
    }

    err = epiphany_load(team, start, count, prog, function, argn, args);
    if (err)
        return err;

    /* Mark as scheduled */
    for (i = start; i < start + count; i++) {
        unsigned offset = ctrl_offset(team, i);
        epiphany->ctrl->status[offset] = STATUS_SCHEDULED;
    }

    /* Ideally a *system* (not host CPU-only) memory barrier here */
    __sync_synchronize();

    return 0;
}

int epiphany_dev_start(struct team *team, int start, int count)
{
    /* Ideally a *system* (not host CPU-only) memory barrier here */
    __sync_synchronize();

    epiphany_start(team, team->start.id + start, count);

    return 0;
}

int epiphany_dev_wait(struct dev *dev, struct team *team)
{
    unsigned i, j = 0;
    int last_rank;
    p_coords_t last_coords;
    bool need_wait = true;
    struct epiphany_dev *data = to_epiphany_dev(dev);

    if (epiphany_last_coords(team, &last_coords))
        return -EINVAL;

    last_rank = p_coords_to_rank(team, &last_coords, 0);

    while (true) {
        need_wait = false;
        for (i = 0; i <= last_rank; i++) {
            unsigned offset = ctrl_offset(team, i);

            switch (data->ctrl->status[offset]) {
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

int epiphany_dev_kill(struct team *team, int start, int count, int signal)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(team->dev);

    if (!epiphany->opened)
        return -EBADF;

    if (start < 0 || count <= 0)
        return -EINVAL;

    if (team->size.id < start + count)
        return -EINVAL;

    switch (signal) {
        case SIGKILL:
            return epiphany_soft_reset(team, start, count);
        default:
            return -ENOSYS;
    }
}
