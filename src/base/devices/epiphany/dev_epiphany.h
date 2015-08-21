#pragma once

#include <e-hal.h>
#include "pal_base.h"
#include "../../pal_base_private.h"

struct epiphany_dev_data {
    bool opened;
    e_epiphany_t dev;
    e_mem_t ctrl;
    e_mem_t args;
};

struct epiphany_args_header {
    uint32_t nargs;
    uint32_t __pad1;
    uint32_t size[P_RUN_MAX_ARGS];
} __attribute__((packed));
