#include "dev_epiphany.h"

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

static void *dev_map_member(struct team *team, int member,
                            unsigned long offset, unsigned long size)
{
    /* Implement me */

    return NULL;
}

static p_mem_t dev_map(struct dev *dev, unsigned long addr, unsigned long size)
{
    return p_mem_err(ENOSYS);
}

static int dev_unmap(struct team *team, p_mem_t *mem)
{
    return -ENOSYS;
}

static p_dev_t dev_init(struct dev *dev, int flags)
{
    return dev;
}

static void dev_fini(struct dev *dev)
{
}

static struct team *dev_open(struct team *team)
{
    return p_ref_err(ENOSYS);
}

static int dev_load(struct team *team, int start, int count,
                   struct prog *prog, const char *function, int argn,
                    const p_arg_t *args)
{
    return -ENOSYS;
}

static int dev_start(struct team *team, int start, int count)
{
    return -ENOSYS;
}

static int dev_wait(struct dev *dev, struct team *team)
{
    return -ENOSYS;
}

static int dev_kill(struct team *team, int start, int count, int signal)
{
    return -ENOSYS;
}

static struct dev_ops epiphany_dev_ops = {
    /* Generic */
    .init = dev_init,
    .fini = dev_fini,
    .open = dev_open,
    .load = dev_load,
    .start = dev_start,
    .wait = dev_wait,
    .kill = dev_kill,
    /* Specific for device */
    .query = dev_query,
    .map_member = dev_map_member,
    .map = dev_map,
    .unmap = dev_unmap,
};

struct epiphany_dev __pal_dev_epiphany = {
    .dev = {
        .dev_ops = &epiphany_dev_ops,
    },
};
