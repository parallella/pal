#include "../bench_tmpl.h"

declare_binary(p_absdiff_f32)
declare_unary(p_abs_f32)
declare_unary(p_acos_f32)
declare_unary(p_acosh_f32)
declare_binary(p_add_f32)
declare_unary(p_asin_f32)
declare_unary(p_asinh_f32)
declare_binary(p_atan2_f32)
declare_unary(p_atan_f32)
declare_unary(p_atanh_f32)
declare_unary(p_cbrt_f32)
declare_unary(p_cos_f32)
declare_unary(p_cosh_f32)
declare_binary(p_div_f32)
declare_binary(p_dot_f32)
declare_unary(p_exp_f32)
void bench_p_ftoi(const struct p_bench_specification *spec)
{
    p_ftoi(spec->mem.i1.p_float, spec->mem.o1.p_int, spec->current_size);
}
declare_unary(p_invcbrt_f32)
declare_unary(p_inv_f32)
declare_unary(p_invsqrt_f32)
void bench_p_itof(const struct p_bench_specification *spec)
{
    p_itof(spec->mem.i1.p_int, spec->mem.o1.p_float, spec->current_size);
}
declare_unary(p_ln_f32)
declare_unary(p_log10_f32)
declare_binary(p_mac_f32)
void bench_p_max_f32(const struct p_bench_specification *spec)
{
    p_max_f32(spec->mem.i1.p_float, spec->mem.i2.p_float, spec->mem.o1.p_int,
              spec->current_size);
}
declare_unary(p_mean_f32)
declare_unary(p_median_f32)
void bench_p_min_f32(const struct p_bench_specification *spec)
{
    p_min_f32(spec->mem.i1.p_float, spec->mem.i2.p_float, spec->mem.o1.p_int,
              spec->current_size);
}
void bench_p_minmax_f32(const struct p_bench_specification *spec)
{
    p_minmax_f32(spec->mem.i1.p_float, spec->mem.i2.p_float,
                 spec->mem.i3.p_float, spec->mem.o1.p_int, spec->mem.o2.p_int,
                 spec->current_size);
}
declare_unary(p_mode_f32)
declare_binary(p_mul_f32)
void bench_p_popcount_u32(const struct p_bench_specification *spec)
{
    p_popcount_u32(spec->mem.i1.p_u32, spec->mem.o1.p_u32, spec->current_size);
}
void bench_p_popcount_u64(const struct p_bench_specification *spec)
{
    p_popcount_u64(spec->mem.i1.p_u64, spec->mem.o1.p_u64, spec->current_size);
}
declare_binary(p_pow_f32)
void bench_p_rand(const struct p_bench_specification *spec) {
    volatile int r;
    for (size_t i = 0; i < spec->current_size; i++) {
        r = p_rand();
    }
    (void) r;
}
declare_unary(p_sin_f32)
void bench_p_sincos_f32(const struct p_bench_specification *spec)
{
    p_sincos_f32(spec->mem.i1.p_float, spec->mem.o1.p_float,
                 spec->mem.o2.p_float, spec->current_size);
}
declare_unary(p_sinh_f32)
declare_unary(p_sort_f32)
declare_unary(p_sqrt_f32)
declare_binary(p_sub_f32)
declare_unary(p_sum_f32)
declare_unary(p_sumsq_f32)
declare_unary(p_tan_f32)
declare_unary(p_tanh_f32)

const struct p_bench_item benchmark_items[] = {
    item(p_absdiff_f32),
    item(p_abs_f32),
    item(p_acos_f32),
    item(p_acosh_f32),
    item(p_add_f32),
    item(p_asin_f32),
    item(p_asinh_f32),
    item(p_atan2_f32),
    item(p_atan_f32),
    item(p_atanh_f32),
    item(p_cbrt_f32),
    item(p_cos_f32),
    item(p_cosh_f32),
    item(p_div_f32),
    item(p_dot_f32),
    item(p_exp_f32),
    item(p_ftoi),
    item(p_invcbrt_f32),
    item(p_inv_f32),
    item(p_invsqrt_f32),
    item(p_itof),
    item(p_ln_f32),
    item(p_log10_f32),
    item(p_mac_f32),
    item(p_max_f32),
    item(p_mean_f32),
    item(p_median_f32),
    item(p_min_f32),
    item(p_minmax_f32),
    item(p_mode_f32),
    item(p_mul_f32),
    item(p_popcount_u32),
    item(p_popcount_u64),
    item(p_pow_f32),
    item(p_rand),
    item(p_sin_f32),
    item(p_sincos_f32),
    item(p_sinh_f32),
    item(p_sort_f32),
    item(p_sqrt_f32),
    item(p_sub_f32),
    item(p_sum_f32),
    item(p_sumsq_f32),
    item(p_tan_f32),
    item(p_tanh_f32),

    { NULL, NULL }
};
