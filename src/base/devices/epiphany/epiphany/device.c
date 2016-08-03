#include "dev_epiphany.h"

static int epiphany_team_coords_to_dev_coords(struct team *team,
                                              const p_coords_t *team_coords,
                                              p_coords_t *dev_coords);

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

static void *dev_map_member(struct team *team, int rank,
                            unsigned long offset, unsigned long size)
{
    uintptr_t coreid, addr;

    p_coords_t team_coords, dev_coords;

    if (p_rank_to_coords(team, rank, &team_coords, 0))
        return NULL;

    if (epiphany_team_coords_to_dev_coords(team, &team_coords, &dev_coords))
        return NULL;

    coreid = (dev_coords.row << 6) | dev_coords.col;
    addr = (coreid << 20) | offset;

    return (void *) addr;
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

static int epiphany_team_coords_to_dev_coords(struct team *team,
                                              const p_coords_t *team_coords,
                                              p_coords_t *dev_coords)
{
    p_coords_t scratch_dev_coords;

    /* Allow NULL ptr argument */
    if (!dev_coords)
        dev_coords = &scratch_dev_coords;

    switch (team->topology) {
    case P_TOPOLOGY_FLAT:
        dev_coords->row =
            team->dev->start.row + team_coords->id / team->dev->size.col;
        dev_coords->col =
            team->dev->start.col + team_coords->id % team->dev->size.col;
        break;
    case P_TOPOLOGY_2D:
        dev_coords->row = team->dev->start.row + team_coords->row;
        dev_coords->col = team->dev->start.col + team_coords->col;
        break;
    default:
        return -EINVAL;
    }

    if (   team->dev->start.row + team->dev->size.row <= dev_coords->row
        || team->dev->start.col + team->dev->size.col <= dev_coords->col)
        return -EINVAL;

    return 0;
}

static void *dev_addr(struct team *team, int rank, uintptr_t offset)
{
    p_coords_t team_coords, dev_coords;
    uintptr_t addr, coreid;

    if (p_rank_to_coords(team, rank, &team_coords, 0))
        return NULL;

    if (epiphany_team_coords_to_dev_coords(team, &team_coords, &dev_coords))
        return NULL;

    coreid = (dev_coords.row << 6) | dev_coords.col;
    addr = (coreid << 20) | offset;

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
    int i, team_size, my_rank;
    int *next_barrier0;
    p_coords_t last_coords;

    team_size = p_team_size(team);

    next_barrier0 = dev_addr(team, 1, (uintptr_t) &team->barrier0);

    team->barrier1++;

    *next_barrier0 = team->barrier1;

    while (team->barrier0 != team->barrier1)
        p_cpu_relax();

    team->barrier1++;
    team->barrier0++;

    for (i = 1; i < team_size; i++) {
        int *bar0 = dev_addr(team, i, (uintptr_t) &team->barrier0);
        *bar0 = team->barrier1;
    }

    _p_fence();

    return 0;
}

static int dev_barrier(struct team *team)
{
    int i, next_rank, my_rank, team_size;
    int *next_barrier0;

    team_size = p_team_size(team);
    my_rank = p_team_rank(team);

    if (team_size < 2)
        return 0;

    if (my_rank == 0)
        return leader_barrier(team);

    next_rank = (my_rank + 1) % (team_size);
    next_barrier0 = dev_addr(team, next_rank, (uintptr_t) &team->barrier0);

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
        .topology = P_TOPOLOGY_2D,
        .start = { .row = 32, .col = 8 },
        .size = { .row = 4, .col = 4 },
    },
};
