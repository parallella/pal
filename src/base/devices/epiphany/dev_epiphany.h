#pragma once

#include <e-hal.h>
#include "pal_base.h"
#include "../../pal_base_private.h"
#include "ctrl.h"

struct epiphany_args_header {
    uint32_t nargs;
    uint32_t __pad1;
    uint32_t size[P_RUN_MAX_ARGS];
} __attribute__((packed));

struct epiphany_dev {
    struct dev dev;     /* Generic device */
    bool initialized;   /* True if dev_early_init() succeeded */
    bool opened;        /* Opened by user call to p_init() */
    e_epiphany_t edev;  /* e-hal device structure */
    struct epiphany_ctrl_mem *ctrl;
    struct epiphany_args_header *args_header;
    int epiphany_fd;    /* File descriptor for epiphany device */
    void *eram;
    void *chip;
};

extern struct epiphany_dev __pal_dev_epiphany;
