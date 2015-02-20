#pragma once

#include <e-hal.h>
#include "pal_base.h"
#include "../../pal_base_private.h"

struct epiphany_dev_data {
    bool opened;
    e_epiphany_t dev;
    e_mem_t ctrl;
};
