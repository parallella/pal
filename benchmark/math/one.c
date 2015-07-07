#include "../bench_tmpl.h"

#ifndef FUNCTION
error "Must define FUNCTION"
#endif

#if IS_BINARY
declare_binary(FUNCTION)
#else
declare_unary(FUNCTION)
#endif

const struct p_bench_item benchmark_items[] = {
    item(FUNCTION),

    { NULL, NULL }
};
