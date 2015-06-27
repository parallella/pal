#pragma once

#include <stdint.h>
#include <stddef.h>
#include <pal.h>

/* For usage, see example benchmark
 */

struct p_benchmark_raw_memory
{
    /*  The pointers used as input/output
     *  while running each individual benchmark.
     *  An example with p_add_f32:
     *  p_add_f32(input_float_first, input_float_second, output_float, ...)
     *  where input_float_first, and input_float_second shall point to different
     *  memory areas, to simulate a real-world like cache scenario.
     *
     *  In practice most of the input_first* pointers point to the same
     *  memory region, but one should keep in mind, ISO C allows for great
     *  flexibility in number representation, and a compiler is conceivable,
     *  certain int bit patterns would be trap representations while
     *  used as uint64_t, and vice versa. If such platforms are to be tested,
     *  different random inputs are needed for int and uint64_t.
     *  The same is true about input_second*, and output* pointers.
     *
     *  Naturally, the output regions are guaranteed not to overlap
     *  with any input region, and are large enough to hold the output of
     *  any currently implemented function.
     *
     *  Additionally, the input_second* pointers, are guaranteed to point
     *  to areas, where no zero integral, or floating point values are
     *  represented, so one does not need to worry about division by zero
     *  while using these as divisors in e.g.: p_div_f32 .
     */

    const float *input_float_first;
    const float *input_float_second;
    float *output_float;
    const double *input_double_first;
    const double *input_double_second;
    double *output_double;
    const char *input_char_first;
    const char *input_char_second;
    char *output_char;
    const short *input_short_first;
    const short *input_short_second;
    short *output_short;
    const int *input_int_first;
    const int *input_int_second;
    int *output_int;
    const long *input_long_first;
    const long *input_long_second;
    long *output_long;
    const uint16_t *input_uint16_t_first;
    const uint16_t *input_uint16_t_second;
    uint16_t *output_uint16_t;
    const uint32_t *input_uint32_t_first;
    const uint32_t *input_uint32_t_second;
    uint32_t *output_uint32_t;
    const uint64_t *input_uint64_t_first;
    const uint64_t *input_uint64_t_second;
    uint64_t *output_uint64_t;
    const uintmax_t *input_uintmax_t_first;
    const uintmax_t *input_uintmax_t_second;
    uintmax_t *output_uintmax_t;
};

struct p_benchmark_specification
{
    struct p_benchmark_raw_memory mem;
    size_t current_size;
};

struct p_benchmark_item
{
    const char *name;
    void (*benchmark)(const struct p_benchmark_specification *);
};

/* The NULL terminated array of items to run */
extern const struct p_benchmark_item benchmark_items[];
