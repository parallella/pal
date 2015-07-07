#include "benchmark.h"
#include <pal.h>

void bench_p_add(const struct p_bench_specification *spec)
{
    p_add_f32(spec->mem.i1.p_float, spec->mem.i2.p_float,
              spec->mem.o2.p_float, spec->current_size);
}

void bench_p_sqrt(const struct p_bench_specification *spec)
{
    p_sqrt_f32(spec->mem.i1.p_float, spec->mem.i2.p_float,
               spec->current_size);
}

const struct p_bench_item benchmark_items[] = {
    {"float addition", bench_p_add},
    {"float square root", bench_p_sqrt},
    {NULL, NULL}};
