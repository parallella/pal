#include "benchmark.h"
#include <pal.h>

void bench_p_add(const struct p_benchmark_specification *spec)
{
    p_add_f32(spec->mem.input_float_first, spec->mem.input_float_second,
              spec->mem.output_float, spec->current_size);
}

void bench_p_sqrt(const struct p_benchmark_specification *spec)
{
    p_sqrt_f32(spec->mem.input_float_first, spec->mem.output_float,
               spec->current_size);
}

const struct p_benchmark_item benchmark_items[] = {
    {"float addition", bench_p_add},
    {"float square root", bench_p_sqrt},
    {NULL, NULL}};
