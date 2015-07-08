#include "benchmark.h"
#include <pal.h>
#include <common.h>

/* Math */
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

/* Image */
#define declare_image(_Fn) declare_image_(_Fn, CONCAT2(bench_, _Fn))
#define declare_image_(_Fn, _BenchName)\
void _BenchName(const struct p_bench_specification *spec)\
{\
    size_t cols = spec->current_size >> 1;\
    size_t rows;\
\
    /* TODO: Let benchmark framework recommend dims before wallclock starts. */\
    while (cols * cols > spec->current_size)\
        cols >>= 1;\
\
    rows = cols;\
\
    while ((rows << 1) * cols < spec->current_size)\
        rows <<= 1;\
\
    _Fn(spec->mem.i1.p_float, spec->mem.i2.p_float,\
        rows, cols);\
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
