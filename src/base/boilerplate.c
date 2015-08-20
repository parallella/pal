
#include <stdio.h>
#include <string.h>
#include "config.h"
#include "pal_base.h"
#include "pal_base_private.h"
#if __epiphany__
#include "devices/epiphany/ctrl.h"
#include <e-lib.h>
#endif


__attribute__((constructor)) void __pal_init(void);
__attribute__((destructor)) void __pal_fini(void);

#if ENABLE_DEV_EPIPHANY
extern struct dev_ops __pal_dev_epiphany_ops;
#endif
#if ENABLE_DEV_GRID
extern struct dev_ops __pal_dev_grid_ops;
#endif
#if ENABLE_DEV_SMP
extern struct dev_ops __pal_dev_smp_ops;
#endif
#if ENABLE_DEV_FPGA
extern struct dev_ops __pal_dev_fpga_ops;
#endif
#if ENABLE_DEV_GPU
extern struct dev_ops __pal_dev_gpu_ops;
#endif
#if ENABLE_DEV_DEMO
extern struct dev_ops __pal_dev_demo_ops;
#endif

#define DEFINE_DEV(Ops) { .dev_ops = Ops, .dev_data = NULL }

/* Defining the table this way statically compile time depends on devs being in
 * the right order.  If we We could do this in the constructor
 * too... */
#ifdef __epiphany__
struct pal_global __pal_global __attribute__ ((section (".data_bank0"))) = {
#else
struct pal_global __pal_global = {
#endif
    .devs = {
        DEFINE_DEV(NULL),
#if ENABLE_DEV_EPIPHANY
        DEFINE_DEV(&__pal_dev_epiphany_ops),
#else
        DEFINE_DEV(NULL),
#endif
#if ENABLE_DEV_GRID
        DEFINE_DEV(&__pal_dev_grid_ops),
#else
        DEFINE_DEV(NULL),
#endif
#if ENABLE_DEV_SMP
        DEFINE_DEV(&__pal_dev_smp_ops),
#else
        DEFINE_DEV(NULL),
#endif
#if ENABLE_DEV_FPGA
        DEFINE_DEV(&__pal_dev_fpga_ops),
#else
        DEFINE_DEV(NULL),
#endif
#if ENABLE_DEV_GPU
        DEFINE_DEV(&__pal_dev_gpu_ops),
#else
        DEFINE_DEV(NULL),
#endif
#if ENABLE_DEV_DEMO
        DEFINE_DEV(&__pal_dev_demo_ops),
#else
        DEFINE_DEV(NULL)
#endif
    },
    .teams_head = NULL,
    .teams_tail = NULL,
    .progs_head = NULL,
    .progs_tail = NULL,

/* We might want to use another PRNG for some platforms */
#ifdef TINYMT32_H
    .random = {
        .status = {0},
        .mat1 = 0,
        .mat2 = 0,
        .tmat = 0
    }
#endif
};

#undef DEFINE_DEV

__attribute__((constructor))
void __pal_init()
{
#if __epiphany__
    /* Platform specifics should probably go into separate file ? */
    // Assume team is entire chip
    const uint32_t coreid = e_get_coreid();
    const uint32_t row = e_group_config.core_row;
    const uint32_t col = e_group_config.core_col;
    const uint32_t rank = row * e_group_config.group_cols + col;
    struct epiphany_ctrl_mem *ctrl =
        (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;

    __pal_global.rank = rank;

    ctrl->status[rank] = STATUS_RUNNING;
#else
    /* NO-OP for now */
    __pal_global.rank = 0;
#endif
}

__attribute__((destructor))
void __pal_fini()
{
#if __epiphany__
    // Assume team is entire chip
    const uint32_t coreid = e_get_coreid();
    const uint32_t row = e_group_config.core_row;
    const uint32_t col = e_group_config.core_col;
    const uint32_t rank = row * e_group_config.group_cols + col;
    struct epiphany_ctrl_mem *ctrl =
        (struct epiphany_ctrl_mem *) CTRL_MEM_EADDR;
    ctrl->status[rank] = STATUS_DONE;
#else
    struct team *team, *next_team;
    struct prog *prog, *next_prog;

    team = __pal_global.teams_head;
    while (team) {
        next_team = team->next;
        // team_fini(team);
        team = next_team;
    }

    prog = __pal_global.progs_head;
    while (prog) {
        next_prog = prog->next;
        // prog_fini(prog);
        prog = next_prog;
    }

    memset(&__pal_global, 0, sizeof(struct pal_global));
#endif
}

