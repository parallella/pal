#include "config.h"
#include "dev_epiphany.h"
#include "pal_base.h"
#include "../../pal_base_private.h"

static p_dev_t dev_init(struct dev *dev, int flags)
{
    return p_ref_err(ENOSYS);
}

static void dev_fini(struct dev *dev)
{
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

static struct team *dev_open(struct dev *dev)
{
    return p_ref_err(ENOSYS);
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
    .wait = dev_wait
};

