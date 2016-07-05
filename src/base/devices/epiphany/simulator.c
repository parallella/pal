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

static int dev_early_init(struct dev *dev)
{
    return epiphany_dev_early_init(dev);
}

static void dev_late_fini(struct dev *dev)
{
    epiphany_dev_late_fini(dev);
}

static int dev_query(struct dev *dev, int property)
{
    return -ENOSYS;
}

static void *dev_map_member(struct team *team, int member,
                            unsigned long offset, unsigned long size)
{
    /* In theory we could support mapping normal memory, but the MMR region
     * must go through the simulator */
    return NULL;
}

static int dev_unmap(struct team *team, void *addr)
{
    return 0;
}

struct dev_ops __pal_dev_epiphany_sim_ops = {
    /* Epiphany generic */
    .init = epiphany_dev_init,
    .fini = epiphany_dev_fini,
    .open = epiphany_dev_open,
    .run = epiphany_dev_run,
    .wait = epiphany_dev_wait,
    /* Specific for simulator */
    .query = dev_query,
    .early_init = dev_early_init,
    .late_fini = dev_late_fini,
    .map_member = dev_map_member,
    .unmap = dev_unmap,
};
