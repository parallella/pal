#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "config.h"
#include "dev_epiphany.h"
#include "ctrl.h"
#include "pal_base.h"
#include <common.h>
#include "../../pal_base_private.h"
#include <e-hal.h>
#include "loader.h"

/* TODO: Obtain from device-tree or ioctl() call */
#define ERAM_SIZE       (32*1024*1024)
#define ERAM_BASE       0x8e000000
#define ERAM_PHY_BASE   0x3e000000
#define CHIP_BASE       0x80800000
#define CHIP_ROWS       4
#define CHIP_COLS       4
#define CORE_MEM_REGION 0x00100000
#define EPIPHANY_DEV "/dev/epiphany"

struct core_map_table {
    off_t   base;
    size_t  size;
} static core_map_table[] = {
    { 0x00000, 0x08000 }, /* SRAM */
    { 0xf0000, 0x01000 }, /* MMR  */
};

static inline struct epiphany_dev *to_epiphany_dev(struct dev *dev)
{
    return container_of(dev, struct epiphany_dev, dev);
}

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
                          dev_data->epiphany_fd, ERAM_PHY_BASE);
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

    ret = mmap_eram(dev);
    if (ret)
        return ret;

    ret = mmap_chip_mem(dev);
    if (ret)
        return ret;

    dev_data->initialized = true;

    return 0;
}

static void dev_late_fini(struct dev *dev)
{
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    if (!dev_data->initialized)
        return;

    munmap(dev_data->eram, ERAM_SIZE);
    /* TODO: unmap chip here */
    close(dev_data->epiphany_fd);

    dev_data->initialized = false;
}

static p_dev_t dev_init(struct dev *dev, int flags)
{
    int err;
    struct epiphany_dev *dev_data = to_epiphany_dev(dev);

    if (!dev_data->initialized)
        return p_ref_err(ENODEV);

    /* Be idempotent if already initialized. It might be a better idea to
     * return EBUSY instead */
    if (dev_data->opened)
        return dev;

    dev_data->ctrl = (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;

    err = e_init(NULL);
    if (err)
        return p_ref_err(EIO);

    err = e_reset_system();
    if (err)
        return p_ref_err(EIO);

    /* Open entire device */
    err = e_open(&dev_data->edev, 0, 0, 4, 4);
    if (err)
        return p_ref_err(ENOMEM);

    /* Clear control structure */
    memset(dev_data->ctrl, 0 , sizeof(*dev_data->ctrl));

    dev_data->opened = 1;

    return dev;
}

static void dev_fini(struct dev *dev)
{
    struct epiphany_dev *data = to_epiphany_dev(dev);

    if (data->opened) {
        e_close(&data->edev);
        data->opened = false;
    }

    e_finalize();
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
    struct epiphany_dev *data = to_epiphany_dev(dev);

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
                   const char *function, int start, int size, int argn,
                   const p_arg_t *args, int flags)
{
    int err;
    int i;
    struct epiphany_dev *data = to_epiphany_dev(dev);

    if (start < 0 || size <= 0)
        return -EINVAL;

    /* Assume we have entire chip for now */
    if (16 < start + size)
        return -EINVAL;

    if (!data || !data->opened)
        return -EBADF;

    /* Copy arguments to device memory (shared ram) */
    {
        size_t argssize = 0, totsize;
        off_t offs = 0;
        struct epiphany_args_header header = { .nargs = argn };

        for (int i = 0; i < argn; i++) {
            argssize += args[i].size;
            header.size[i] = args[i].size;
        }

        if (argssize > EPIPHANY_DEV_MAX_ARGS_SIZE)
            return -ENOMEM;

        totsize = sizeof(header) + argssize;
        totsize = (totsize + 7) & (~7);

        /* "Allocate" memory in shared RAM. TODO: Hard coded address */
        data->args_header =
            (struct epiphany_args_header *) (ARGS_MEM_END_EADDR - totsize);

        memcpy(data->args_header, &header, sizeof(header));
        uint8_t *argsp = (uint8_t *) &data->args_header[1];
        for (int i = 0; i < argn; i++) {
            memcpy(&argsp[offs], args[i].ptr, args[i].size);
            offs += args[i].size;
        }

        /* Write offset in control structure */
        data->ctrl->argsoffset = totsize;
    }

    epiphany_soft_reset(team, start, size);

    err = epiphany_load(team, prog, start, size, flags);
    if (err)
        return err;

    /* Mark as scheduled */
    for (i = start; i < start + size; i++)
        data->ctrl->status[i] = STATUS_SCHEDULED;

    epiphany_start(team, start, size, flags);

    return 0;
}

static int dev_wait(struct dev *dev, struct team *team)
{
    unsigned i;
    bool need_wait = true;
    struct epiphany_dev *data = to_epiphany_dev(dev);

    while (true) {
        need_wait = false;
        for (i = 0; i < 16; i++) {
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

        /* Don't burn CPU. Need HW/Kernel support for blocking wait */
        usleep(1000);
    }

    return 0;
}

static struct dev_ops epiphany_dev_ops = {
    .init = dev_init,
    .fini = dev_fini,
    .query = dev_query,
    .open = dev_open,
    .run = dev_run,
    .wait = dev_wait,
    .early_init = dev_early_init,
    .late_fini = dev_late_fini,
};

struct epiphany_dev __pal_dev_epiphany = {
    .dev = {
        .dev_ops = &epiphany_dev_ops,
    },
};
