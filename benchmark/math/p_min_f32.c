#include "../bench_tmpl.h"

void bench_p_min_f32(const struct p_bench_specification *spec)
{
    p_min_f32(spec->mem.i1.p_float, spec->mem.i2.p_float, spec->mem.o1.p_int,
              spec->current_size);
}

const struct p_bench_item benchmark_items[] = {
    item(p_min_f32),

    { NULL, NULL }
};
