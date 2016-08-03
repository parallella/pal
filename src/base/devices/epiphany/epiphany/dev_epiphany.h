#pragma once

#include <pal.h>
#include "../../../pal_base_private.h"

typedef struct es_state_ es_state;

struct epiphany_dev {
    struct dev dev;     /* Generic device */
    uint32_t eram_base;
    uint32_t eram_size;
};

extern struct epiphany_dev __pal_dev_epiphany;
