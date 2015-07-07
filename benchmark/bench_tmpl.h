#include "benchmark.h"
#include <pal.h>
#include <common.h>

#define declare_unary(_Fn) declare_unary_(_Fn, CONCAT2(bench_, _Fn))
#define declare_unary_(_Fn, _BenchName)\
void _BenchName(const struct p_bench_specification *spec)\
{\
    _Fn(spec->mem.i1.p_float, spec->mem.o1.p_float, spec->current_size);\
}

#define declare_binary(_Fn) declare_binary_(_Fn, CONCAT2(bench_, _Fn))
#define declare_binary_(_Fn, _BenchName)\
void _BenchName(const struct p_bench_specification *spec)\
{\
    _Fn(spec->mem.i1.p_float, spec->mem.i2.p_float,\
        spec->mem.o1.p_float, spec->current_size);\
}

#define item(_Fn) { STRING(_Fn), CONCAT2(bench_, _Fn) }

/*
 * Example usage:
 *
 * #include "bench_tmpl.h"
 *
 * declare_unary(p_abs_f32)
 *
 * void bench_p_max_f32(const struct p_bench_specification *spec)
 * {
 *     p_max_f32(spec->mem.i1.p_float, spec->mem.i2.p_float, spec->mem.o1.p_i32,
 *               spec->current_size);
 * }
 *
 * const struct p_bench_item benchmark_items[] = {
 *     item(p_abs_f32),
 *     item(p_max_f32),
 *     { NULL, NULL }
 * };
 */
