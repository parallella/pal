#pragma once

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pal.h>

/* For usage, see example benchmark
 */

union p_bench_param {
    /* FP types */
    float *p_float;
    double *p_double;

    /* Boolean */
    bool *p_bool;

    /* Unsigned types */
    unsigned int *p_uint;
    uint8_t  *p_u8;
    uint16_t *p_u16;
    uint32_t *p_u32;
    uint64_t *p_u64;

    /* Signed types */
    int     *p_int;
    int8_t  *p_i8;
    int16_t *p_i16;
    int32_t *p_i32;
    int64_t *p_i64;

    /* Fallback, but it might be a better to add the type here instead. */
    void *p_void;
};

struct p_bench_raw_memory
{
    /*  The pointers used as input/output while running each individual
     *  benchmark.
     *  An example with p_add_f32:
     *
     *  p_add_f32(i1.p_float, i2.p_float, o1.p_float, ...)
     *
     *  where i1, and i2 shall point to different memory areas, to simulate a
     *  real-world like cache scenario.
     *
     *  In practice most of the i* pointers point to the same memory region,
     *  but one should keep in mind, ISO C allows for great flexibility in
     *  number representation, and a compiler is conceivable, certain int bit
     *  patterns would be trap representations while used as uint64_t, and vice
     *  versa. If such platforms are to be tested, different random inputs are
     *  needed for int and uint64_t.  The same is true about i*, and o*
     *  pointers.
     *
     *  Naturally, the output regions are guaranteed not to overlap with any
     *  input region, and are large enough to hold the output of any currently
     *  implemented function.
     *
     *  Additionally, the i{2..}* pointers, are guaranteed to point to areas,
     *  where no zero integral, or floating point values are represented, so
     *  one does not need to worry about division by zero while using these as
     *  divisors in e.g.: p_div_f32.
     */

    union {
        const union p_bench_param i1;
        union p_bench_param i1_w;
    };

    union {
        const union p_bench_param i2;
        union p_bench_param i2_w;
    };

    union {
        const union p_bench_param i3;
        union p_bench_param i3_w;
    };

    union {
        const union p_bench_param i4;
        union p_bench_param i4_w;
    };

    union p_bench_param o1;
    union p_bench_param o2;
    union p_bench_param o3;
    union p_bench_param o4;
};

struct p_bench_specification
{
    struct p_bench_raw_memory mem;
    size_t current_size;
};

struct p_bench_item
{
    const char *name;
    void (*benchmark)(const struct p_bench_specification *);
};

/* The NULL terminated array of items to run */
extern const struct p_bench_item benchmark_items[];
