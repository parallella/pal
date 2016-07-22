#pragma once

#include "pal_base.h"
#include "../../pal_base_private.h"
#include "ctrl.h"

typedef struct es_state_ es_state;

struct epiphany_args_header {
    uint32_t nargs;
    uint32_t __pad1;
    uint32_t size[P_RUN_MAX_ARGS];
} __attribute__((packed));

struct epiphany_dev;

/* Ops used by loader */
struct epiphany_loader_ops {
    uint32_t (*reg_read)(struct epiphany_dev *, uintptr_t, uintptr_t);
    void (*reg_write)(struct epiphany_dev *, uintptr_t, uintptr_t, uint32_t);

    void (*mem_read)(struct epiphany_dev *, void *, uintptr_t, size_t);
    void (*mem_write)(struct epiphany_dev *, uintptr_t, const void *, size_t);
};

struct epiphany_dev {
    struct dev dev;     /* Generic device */
    bool initialized;   /* True if dev_early_init() succeeded */
    bool opened;        /* Opened by user call to p_init() */
    struct epiphany_ctrl_mem *ctrl;
    struct epiphany_args_header *args_header;
    int epiphany_fd;    /* File descriptor for epiphany device */
    void *eram;
    void *chip;
    struct epiphany_loader_ops loader_ops;

    es_state *esim; /* ESIM handle */

    unsigned rows;
    unsigned cols;
    unsigned row_base;
    unsigned col_base;
    unsigned sram_size;
    unsigned eram_base;
    unsigned eram_size;
};

extern struct epiphany_dev __pal_dev_epiphany;
#ifdef ENABLE_DEV_EPIPHANY_SIM
extern struct dev_ops __pal_dev_epiphany_sim_ops;
#endif
