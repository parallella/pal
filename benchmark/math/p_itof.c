#include "../bench_tmpl.h"

void bench_p_itof(const struct p_bench_specification *spec)
{
    p_itof(spec->mem.i1.p_int, spec->mem.o1.p_float, spec->current_size);
}

const struct p_bench_item benchmark_items[] = {
    item(p_itof),

    { NULL, NULL }
};
