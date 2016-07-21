#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdbool.h>

#include <esim.h>

#include "config.h"
#include "dev_epiphany.h"
#include "ctrl.h"
#include "pal_base.h"
#include <common.h>
#include "../../pal_base_private.h"
#include "loader.h"
#include "generic.h"

static uint32_t reg_read(struct epiphany_dev *, uintptr_t, uintptr_t);
static void reg_write(struct epiphany_dev *, uintptr_t, uintptr_t, uint32_t);
static void mem_read(struct epiphany_dev *, void *, uintptr_t, size_t);
static void mem_write(struct epiphany_dev *, uintptr_t, const void *, size_t);

static int dev_early_init(struct dev *dev)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);
    struct epiphany_loader_ops ops = {
        .reg_read = reg_read,
        .reg_write = reg_write,
        .mem_read = mem_read,
        .mem_write = mem_write,
    };
    epiphany->loader_ops = ops;

    return epiphany_dev_early_init(dev);
}

static void dev_late_fini(struct dev *dev)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    es_client_disconnect(epiphany->esim, true);

    epiphany_dev_late_fini(dev);
}

static p_dev_t dev_init(struct dev *dev, int flags)
{
    int err;
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    if (!epiphany->initialized)
        return p_ref_err(ENODEV);

    /* Be idempotent if already initialized. It might be a better idea to
     * return EBUSY instead */
    if (epiphany->opened)
        return dev;

    if (es_client_connect(&epiphany->esim, NULL))
        return p_ref_err(EIO);

    epiphany->eram = (void *) es_client_get_raw_pointer(epiphany->esim,
                                                        0x8e000000,
                                                        32*1024*1024);
    epiphany->ctrl = (struct epiphany_ctrl_mem *)
                     ((uint8_t *) epiphany->eram + CTRL_MEM_OFFSET);

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

static void *dev_map(struct dev *dev, unsigned long addr, unsigned long size)
{
    /* HACK */

    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    if (addr < 0x8e000000 || 32*1024*1024 < addr - 0x8e000000 + size)
        return NULL;

    {
        uintptr_t offset = addr - 0x8e000000;
        uint8_t *ptr = epiphany->eram;
        return &ptr[offset];
    }
}

static void *dev_map_raw(struct dev *dev, unsigned long addr, unsigned long size)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    return (void *) es_client_get_raw_pointer(epiphany->esim, addr, size);
}

static uint32_t reg_read(struct epiphany_dev *epiphany, uintptr_t base,
                         uintptr_t offset)
{
    uint32_t val;

    es_mem_load(epiphany->esim, base + offset, sizeof(val), (uint8_t *) &val);

    return val;
}

static void reg_write(struct epiphany_dev *epiphany, uintptr_t base,
                      uintptr_t offset, uint32_t val)
{
    es_mem_store(epiphany->esim, base + offset, sizeof(val), (uint8_t *) &val);
}

static void mem_read(struct epiphany_dev *epiphany, void *dst, uintptr_t src,
                     size_t n)
{
    es_mem_load(epiphany->esim, src, n, (uint8_t *) dst);
}

static void mem_write(struct epiphany_dev *epiphany, uintptr_t dst,
                      const void *src, size_t n)
{
    es_mem_store(epiphany->esim, dst, n, (uint8_t *) src);
}

struct dev_ops __pal_dev_epiphany_sim_ops = {
    /* Epiphany generic */
    .fini = epiphany_dev_fini,
    .open = epiphany_dev_open,
    .run = epiphany_dev_run,
    .wait = epiphany_dev_wait,
    /* Specific for simulator */
    .init = dev_init,
    .query = dev_query,
    .early_init = dev_early_init,
    .late_fini = dev_late_fini,
    .map_member = dev_map_member,
    .map = dev_map,
    .unmap = dev_unmap,
    ._map_raw = dev_map_raw,
};
