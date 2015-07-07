#include "../bench_tmpl.h"

void bench_p_popcount_u64(const struct p_bench_specification *spec)
{
    p_popcount_u64(spec->mem.i1.p_u64, spec->mem.o1.p_u64, spec->current_size);
}

const struct p_bench_item benchmark_items[] = {
    item(p_popcount_u64),

    { NULL, NULL }
};
