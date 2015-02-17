#include "config.h"
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
    return -ENOSYS;
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

