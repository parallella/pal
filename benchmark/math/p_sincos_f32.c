#include "../bench_tmpl.h"

void bench_p_sincos_f32(const struct p_bench_specification *spec)
{
    p_sincos_f32(spec->mem.i1.p_float, spec->mem.o1.p_float,
                 spec->mem.o2.p_float, spec->current_size);
}

const struct p_bench_item benchmark_items[] = {
    item(p_sincos_f32),

    { NULL, NULL }
};
