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

/* TODO: Obtain from device-tree or ioctl() call */
#define ERAM_SIZE       (32*1024*1024)
#define ERAM_BASE       0x8e000000
#define ERAM_PHY_BASE   0x3e000000
#define CHIP_BASE       0x80800000
#define CHIP_ROWS       4
#define CHIP_COLS       4
#define SRAM_SIZE       0x8000
#define SRAM_BASE       0x0
#define CORE_MEM_REGION 0x00100000
#define EPIPHANY_DEV "/dev/epiphany/mesh0"

struct core_map_table {
    off_t   base;
    size_t  size;
} static core_map_table[] = {
    { 0x00000, 0x08000 }, /* SRAM */
    { 0xf0000, 0x01000 }, /* MMR  */
};

/* Returns 0 on success */
static int mmap_eram(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);
    long page_size;
    unsigned npages;
    unsigned char dummy;
    uintptr_t addr;
    int ret;

    page_size = sysconf(_SC_PAGESIZE);

    if (0 > page_size)
        return -EINVAL;

    /* Check that address space is not already mapped */
    addr = ERAM_BASE;
    npages = ERAM_SIZE / page_size;
    for (int i = 0; i < npages; i++, addr += page_size) {
again:
        ret = mincore((void *) addr, page_size, &dummy);

        if (ret == -1 && errno == EAGAIN)
            goto again;

        /* This is what we want (page is unmapped) */
        if (ret == -1 && errno == ENOMEM)
            continue;

        return -ENOMEM;
    }

    dev_data->eram = mmap((void *) ERAM_BASE, ERAM_SIZE,
                          PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED,
                          dev_data->epiphany_fd, ERAM_BASE);
    if (dev_data->eram == MAP_FAILED)
        return -errno;

    return 0;
}

/* Returns 0 on success */
static int mmap_chip_mem(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);
    void *ptr;
    long page_size;
    unsigned npages;
    unsigned char dummy;
    uintptr_t addr;
    int row, col, region;
    int ret;

    page_size = sysconf(_SC_PAGESIZE);

    if (0 > page_size)
        return -EINVAL;

    /* Check that address space is not already mapped */
    for (row = 0; row < CHIP_ROWS; row++) {
        for (col = 0; col < CHIP_COLS; col++, addr += CORE_MEM_REGION) {
            for (region = 0; region < ARRAY_SIZE(core_map_table); region++) {
                addr = CHIP_BASE + (64 * row + col) * CORE_MEM_REGION +
                       core_map_table[region].base;
                npages = core_map_table[region].size / page_size;
                for (int i = 0; i < npages; i++, addr += page_size) {
again:
                    ret = mincore((void *) addr, page_size, &dummy);

                    if (ret == -1 && errno == EAGAIN)
                        goto again;

                    /* This is what we want (page is unmapped) */
                    if (ret == -1 && errno == ENOMEM)
                        continue;

                    return -ENOMEM;
                }
            }
        }
    }

    /* Allow R/W access to core regions */
    /* Do 1:1 mapping w/ chip for now */
    addr = CHIP_BASE;
    for (row = 0; row < CHIP_ROWS; row++) {
        for (col = 0; col < CHIP_COLS; col++, addr += CORE_MEM_REGION) {
            for (region = 0; region < ARRAY_SIZE(core_map_table); region++) {
                addr = CHIP_BASE + (64 * row + col) * CORE_MEM_REGION +
                       core_map_table[region].base;

                ptr = mmap((void *) addr, core_map_table[region].size,
                           PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED,
                           dev_data->epiphany_fd, addr);

                if (ptr == MAP_FAILED)
                    return -errno;
                if (ptr != (void *) addr)
                    return -ENOMEM;
            }
        }
    }

    dev_data->chip = (void *) CHIP_BASE;
    return 0;
}

p_dev_t dev_init(struct dev *dev, int flags)
{
    int ret;
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);

    if (!epiphany->initialized)
        return p_ref_err(ENODEV);

    /* Be idempotent if already initialized. It might be a better idea to
     * return EBUSY instead */
    if (epiphany->opened)
        return dev;

    epiphany->epiphany_fd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
    if (epiphany->epiphany_fd == -1)
        return p_ref_err(errno);

    epiphany->sram_size = SRAM_SIZE;
    epiphany->eram_base = ERAM_BASE;
    epiphany->eram_size = ERAM_SIZE;

    ret = mmap_eram(dev);
    if (ret)
        return p_ref_err(-ret);

    ret = mmap_chip_mem(dev);
    if (ret)
        return p_ref_err(-ret);

    epiphany->ctrl = (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;

#if 0
    /* I don't think this is needed here, soft reset on load should be enough */
    ret = epiphany_reset_system(epiphany);
    if (ret)
        return p_ref_err(-ret);
#endif

    /* Clear control structure */
    memset(epiphany->ctrl, 0 , sizeof(*epiphany->ctrl));

    epiphany->opened = true;

    return dev;
}

static void dev_fini(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    if (!dev_data->initialized)
        return;

    munmap(dev_data->eram, ERAM_SIZE);
    /* TODO: unmap chip here */
    close(dev_data->epiphany_fd);

    epiphany_dev_fini(dev);
}

static void *dev_map_member(struct team *team, int member,
                            unsigned long offset, unsigned long size)
{
    /* HACK */
    unsigned coreid, row, col;
    uintptr_t addr;

    if (member < 0 || 15 < member)
        return NULL;

    if (offset >= (1 << 20) || (offset + size) > (1 << 20))
        return NULL;

    row = member / 4;
    col = member % 4;

    coreid = 0x808 + (row << 6 | col << 0);

    addr = coreid << 20 | offset;

    return (void *) addr;
}

static void *dev_map_raw(struct dev *dev, unsigned long addr, unsigned long size)
{
    /* HACK */
    return (void *) addr;
}


static uint32_t reg_read(struct epiphany_dev *epiphany,
                         uintptr_t base, uintptr_t offset)
{
    volatile uint32_t *reg = (uint32_t *) ((uintptr_t) base + offset);
    return *reg;
}

static void reg_write(struct epiphany_dev *epiphany, uintptr_t base,
                      uintptr_t offset, uint32_t val)
{
    volatile uint32_t *reg = (uint32_t *) ((uintptr_t) base + offset);
    *reg = val;
}

static void mem_read(struct epiphany_dev *dev, void *dst, uintptr_t src,
                     size_t n)
{
    memcpy((void *) dst, (void *) src, n);
}
static void mem_write(struct epiphany_dev *dev, uintptr_t dst, const void *src,
                      size_t n)
{
    memcpy((void *) dst, (void *) src, n);
}

static ssize_t dev_mem_write(p_mem_t *mem, const void *src, off_t offset,
                             size_t nb, int flags)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(mem->dev);
    uintptr_t addr = (uintptr_t) mem->ref;

    /* HACK */
    //if (addr < 0x8e000000 || 32*1024*1024 < addr - 0x8e000000 + nb)
    //    return -EINVAL;

    mem_write(epiphany, addr + offset, src, nb);

    return nb;
}

static ssize_t dev_mem_read(p_mem_t *mem, void *dst, off_t offset,
                            size_t nb, int flags)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(mem->dev);
    uintptr_t addr = (uintptr_t) mem->ref;

    /* HACK */
    //if (addr < 0x8e000000 || 32*1024*1024 < addr - 0x8e000000 + nb)
    //    return -EINVAL;

    mem_read(epiphany, dst, addr + offset, nb);

    return nb;
}

static struct mem_ops dev_mem_ops = {
    .read = dev_mem_read,
    .write = dev_mem_write,
};

static p_mem_t dev_map(struct dev *dev, unsigned long addr, unsigned long size)
{
    struct epiphany_dev *epiphany = to_epiphany_dev(dev);
    p_mem_t mem;

    /* HACK */
    //if (addr < 0x8e000000 || 32*1024*1024 < addr - 0x8e000000 + size)
    //    return p_mem_err(EINVAL);

    mem.ref = (void *) addr;
    mem.size = size;
    mem.ops = &dev_mem_ops;
    mem.dev = dev;

    return mem;
}

static int dev_unmap(struct team *team, p_mem_t *mem)
{
    /* HACK */

    return 0;
}

static struct dev_ops epiphany_dev_ops = {
    /* Generic */
    .early_init = epiphany_dev_early_init,
    .late_fini = epiphany_dev_late_fini,
    .open = epiphany_dev_open,
    .load = epiphany_dev_load,
    .start = epiphany_dev_start,
    .wait = epiphany_dev_wait,
    .kill = epiphany_dev_kill,
    /* Specific for device */
    .init = dev_init,
    .fini = dev_fini,
    .query = epiphany_dev_query,
    .map_member = dev_map_member,
    .map = dev_map,
    .unmap = dev_unmap,
    ._map_raw = dev_map_raw,
};

struct epiphany_dev __pal_dev_epiphany = {
    .dev = {
        .dev_ops = &epiphany_dev_ops,
        .topology = P_TOPOLOGY_2D,
        .start = { .row = 32, .col = 8 },
        .size = { .row = 4, .col = 4 },
    },
    .loader_ops = {
        reg_read,
        reg_write,
        mem_read,
        mem_write,
    },
};
