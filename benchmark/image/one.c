#include "../bench_tmpl.h"

#ifndef FUNCTION
error "Must define FUNCTION"
#endif

declare_image(FUNCTION)

const struct p_bench_item benchmark_items[] = {
    item(FUNCTION),

    { NULL, NULL }
};
