#include "dev_epiphany.h"
#include <e-lib.h>

extern const e_group_config_t e_group_config;

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

static void *dev_addr(struct team *team, int rank, uintptr_t offset)
{
    unsigned row = e_group_config.group_row + rank / e_group_config.group_cols;
    unsigned col = e_group_config.group_col + rank % e_group_config.group_cols;
    unsigned coreid = (row << 6) | col;
    uintptr_t addr = (coreid << 20) | offset;
    return (void *) addr;
}

static int dev_mutex_lock (struct team *team, p_mutex_t *mutex)
{
    int *global_mutex = dev_addr(team, 0, (uintptr_t) &mutex->mutex);
    while (_p_testset(global_mutex, 1))
        p_cpu_relax();

    return 0;
}

static int dev_mutex_trylock(struct team *team, p_mutex_t *mutex)
{
    int *global_mutex = dev_addr(team, 0, (uintptr_t) &mutex->mutex);

    return _p_testset(global_mutex, 1) ? -EBUSY : 0;
}

static int dev_mutex_unlock(struct team *team, p_mutex_t *mutex)
{
    int *global_mutex = dev_addr(team, 0, (uintptr_t) &mutex->mutex);

    _p_lock_release(global_mutex);

    return 0;
}

static int leader_barrier(struct team *team)
{
    int i;
    int next = (team->rank.id + 1) % team->size.id;
    int *next_barrier0 = dev_addr(team, next, (uintptr_t) &team->barrier0);

    team->barrier1++;

    *next_barrier0 = team->barrier1;

    while (team->barrier0 != team->barrier1)
        p_cpu_relax();

    team->barrier1++;
    team->barrier0++;

    for (i = 1; i < team->size.id; i++) {
        int *bar0 = dev_addr(team, i, (uintptr_t) &team->barrier0);
        *bar0 = team->barrier1;
    }

    _p_fence();

    return 0;
}

static int dev_barrier(struct team *team)
{
    if (team->size.id < 2)
        return 0;

    if (team->rank.id == 0)
        return leader_barrier(team);

    int next = (team->rank.id + 1) % team->size.id;
    int *next_barrier0 = dev_addr(team, next, (uintptr_t) &team->barrier0);

    while (team->barrier0 == team->barrier1)
        p_cpu_relax();

    team->barrier1++;

    *next_barrier0 = team->barrier1;

    while (team->barrier0 == team->barrier1)
        p_cpu_relax();

    team->barrier1++;

    _p_fence();

    return 0;
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

    .mutex_lock = dev_mutex_lock,
    .mutex_unlock = dev_mutex_unlock,
    .mutex_trylock = dev_mutex_trylock,
    .barrier = dev_barrier,
};

struct epiphany_dev __pal_dev_epiphany = {
    .dev = {
        .dev_ops = &epiphany_dev_ops,
    },
};
