#pragma once
#include <pal.h>

#include "config.h"
#include "dev_epiphany.h"
#include "ctrl.h"
#include "../../pal_base_private.h"
#include "loader.h"

static inline struct epiphany_dev *to_epiphany_dev(struct dev *dev)
{
    return container_of(dev, struct epiphany_dev, dev);
}

int epiphany_dev_early_init(struct dev *dev);

void epiphany_dev_late_fini(struct dev *dev);

p_dev_t epiphany_dev_init(struct dev *dev, int flags);

void epiphany_dev_fini(struct dev *dev);

int epiphany_dev_query(struct dev *dev, int property);

struct team *epiphany_dev_open(struct team *team);

int epiphany_dev_load(struct team *team, int start, int count,
                      struct prog *prog, const char *function,
                      int argn, const p_arg_t *args);

int epiphany_dev_start(struct team *team, int start, int count);

int epiphany_dev_wait(struct dev *dev, struct team *team);

int epiphany_dev_kill(struct team *team, int start, int count, int signal);
