#pragma once

#include <pal.h>
#include "../../../pal_base_private.h"

typedef struct es_state_ es_state;

struct epiphany_dev {
    struct dev dev;     /* Generic device */
};

extern struct epiphany_dev __pal_dev_epiphany;
