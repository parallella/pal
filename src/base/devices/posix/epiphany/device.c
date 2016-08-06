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

static int dev_early_init(struct dev *dev)
{
    int ret;
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    dev_data->epiphany_fd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
    if (dev_data->epiphany_fd == -1)
        return -errno;

    dev_data->sram_size = SRAM_SIZE;
    dev_data->eram_base = ERAM_BASE;
    dev_data->eram_size = ERAM_SIZE;

    ret = mmap_eram(dev);
    if (ret)
        return ret;

    ret = mmap_chip_mem(dev);
    if (ret)
        return ret;

    return epiphany_dev_early_init(dev);
}

static void dev_late_fini(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    if (!dev_data->initialized)
        return;

    munmap(dev_data->eram, ERAM_SIZE);
    /* TODO: unmap chip here */
    close(dev_data->epiphany_fd);

    epiphany_dev_late_fini(dev);
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

static void *dev_map_member(struct team *team, int member,
                            unsigned long offset, unsigned long size)
{
    /* HACK */
    unsigned coreid, row, col;
    uintptr_t addr;

    if (member < 0 || 15 < member)
        return NULL;

    if (offset >= (1 << 20) || (offset + size) > (1 << 20))

    row = member / 4;
    col = member % 4;

    coreid = 0x808 + (row << 6 | col << 0);

    addr = coreid << 20 | offset;

    return (void *) addr;
}

static void *dev_map(struct dev *dev, unsigned long addr, unsigned long size)
{
    /* HACK */
    return (void *) addr;
}

static int dev_unmap(struct team *team, void *addr)
{
    /* HACK */

    return 0;
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

static struct dev_ops epiphany_dev_ops = {
    /* Generic */
    .init = epiphany_dev_init,
    .fini = epiphany_dev_fini,
    .open = epiphany_dev_open,
    .load = epiphany_dev_load,
    .start = epiphany_dev_start,
    .wait = epiphany_dev_wait,
    .kill = epiphany_dev_kill,
    /* Specific for device */
    .query = dev_query,
    .early_init = dev_early_init,
    .late_fini = dev_late_fini,
    .map_member = dev_map_member,
    .map = dev_map,
    .unmap = dev_unmap,
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
