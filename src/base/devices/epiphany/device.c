#include <string.h>
#include "config.h"
#include "dev_epiphany.h"
#include "pal_base.h"
#include "../../pal_base_private.h"
#include <e-hal.h>
#include <e-loader.h>

static p_dev_t dev_init(struct dev *dev, int flags)
{
    int err;
    struct epiphany_dev_data *data;

    /* Be idempotent if already initialized. It might be a better idea to
     * return EBUSY instead */
    if (dev->dev_data)
        return dev;


    err = e_init(NULL);
    if (err)
        return p_ref_err(EIO);
    err = e_reset_system();
    if (err)
        return p_ref_err(EIO);

    data = malloc(sizeof(*data));
    if (!data)
        return p_ref_err(ENOMEM);

    memset(data, 0, sizeof(*data));

    /* Open entire device */
    err = e_open(&data->dev, 0, 0, 4, 4);
    if (err) {
        free(data);
        return p_ref_err(EIO);
    }

    data->opened = 1;

    dev->dev_data = (void *) data;

    return dev;
}

static void dev_fini(struct dev *dev)
{
    struct epiphany_dev_data *data =
        (struct epiphany_dev_data *) dev->dev_data;

    if (!data)
        return;

    if (data->opened)
        e_close(&data->dev);

    e_finalize();

    free(dev->dev_data);
    dev->dev_data = NULL;
}

static int dev_query(struct dev *dev, int property)
{
    if (!dev)
        return -EINVAL;

    switch (property) {
    case P_PROP_TYPE:
        return P_DEV_EPIPHANY;
    case P_PROP_NODES:
        return 16;
    case P_PROP_TOPOLOGY:
        return 2;
    case P_PROP_ROWS:
        return 4;
    case P_PROP_COLS:
        return 4;
    case P_PROP_PLANES:
        return 4;
    case P_PROP_CHIPROWS:
        return 4;
    case P_PROP_CHIPCOLS:
        return 4;
    case P_PROP_SIMD:
        return 1;
    case P_PROP_MEMSIZE:
        return 32768;
    case P_PROP_MEMBASE:
        return 0x80800000;
    case P_PROP_VERSION:
        return 0xdeadbeef;
    case P_PROP_MEMARCH:
    case P_PROP_WHOAMI:
        return -ENOSYS;
    }
    return -EINVAL;
}

static struct team *dev_open(struct dev *dev, struct team *team, int start,
        int count)
{
    int err;
    struct epiphany_dev_data *data = dev->dev_data;

    /* Only support opening entire chip for now */
    if (start != 0 || count != 16)
        return p_ref_err(EINVAL);

    /* Open was done in init */
    if (!data->opened)
        return p_ref_err(EBADF);

    team->dev = dev;

    return team;
}

static int dev_run(struct dev *dev, struct team *team, struct prog *prog,
        int start, int size, int argn, char *args[], int flags)
{
    int err;
    unsigned i;
    struct epiphany_dev_data *data = dev->dev_data;

    if (start < 0 || size <= 0)
        return -EINVAL;

    /* Assume we have entire chip for now */
    if (16 < start + size)
        return -EINVAL;

    if (!data || !data->opened)
        return -EBADF;

    for (i = start; i < start + size; i++) {
        err = e_load(prog->path, &data->dev, i / 4, i % 4, E_FALSE);
        if (err)
            return -EIO;
    }
    for (i = start; i < start + size; i++) {
        err = e_start(&data->dev, i / 4, i % 4);
        if (err)
            return -EIO;
    }

    return 0;

}


static int dev_wait(struct dev *dev, struct team *team)
{
    return -ENOSYS;
}

struct dev_ops __pal_dev_epiphany_ops = {
    .init = dev_init,
    .fini = dev_fini,
    .query = dev_query,
    .open = dev_open,
    .run = dev_run,
    .wait = dev_wait
};

