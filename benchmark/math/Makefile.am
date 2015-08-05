AUTOMAKE_OPTIONS = foreign

LDADD = $(top_builddir)/benchmark/libbenchmark.la $(top_builddir)/src/libpal.la

# In alphabetic order
ALLPROGRAMS := \
    bench_p_absdiff_f32 \
    bench_p_abs_f32 \
    bench_p_acos_f32 \
    bench_p_acosh_f32 \
    bench_p_add_f32 \
    bench_p_asin_f32 \
    bench_p_asinh_f32 \
    bench_p_atan2_f32 \
    bench_p_atan_f32 \
    bench_p_atanh_f32 \
    bench_p_cbrt_f32 \
    bench_p_cos_f32 \
    bench_p_cosh_f32 \
    bench_p_div_f32 \
    bench_p_dot_f32 \
    bench_p_exp_f32 \
    bench_p_ftoi \
    bench_p_invcbrt_f32 \
    bench_p_inv_f32 \
    bench_p_invsqrt_f32 \
    bench_p_itof \
    bench_p_ln_f32 \
    bench_p_log10_f32 \
    bench_p_mac_f32 \
    bench_p_max_f32 \
    bench_p_mean_f32 \
    bench_p_median_f32 \
    bench_p_min_f32 \
    bench_p_mode_f32 \
    bench_p_mul_f32 \
    bench_p_popcount_u32 \
    bench_p_popcount_u64 \
    bench_p_pow_f32 \
    bench_p_rand \
    bench_p_sin_f32 \
    bench_p_sincos_f32 \
    bench_p_sinh_f32 \
    bench_p_sort_f32 \
    bench_p_sqrt_f32 \
    bench_p_sub_f32 \
    bench_p_sum_f32 \
    bench_p_sumsq_f32 \
    bench_p_tan_f32 \
    bench_p_tanh_f32

if HOST_IS_EPIPHANY
noinst_PROGRAMS = $(ALLPROGRAMS)
else
noinst_PROGRAMS = $(ALLPROGRAMS)
endif

bench_p_absdiff_f32_SOURCES     = one.c
bench_p_abs_f32_SOURCES         = one.c
bench_p_acos_f32_SOURCES        = one.c
bench_p_acosh_f32_SOURCES       = one.c
bench_p_add_f32_SOURCES         = one.c
bench_p_asin_f32_SOURCES        = one.c
bench_p_asinh_f32_SOURCES       = one.c
bench_p_atan2_f32_SOURCES       = one.c
bench_p_atan_f32_SOURCES        = one.c
bench_p_atanh_f32_SOURCES       = one.c
bench_p_cbrt_f32_SOURCES        = one.c
bench_p_cos_f32_SOURCES         = one.c
bench_p_cosh_f32_SOURCES        = one.c
bench_p_div_f32_SOURCES         = one.c
bench_p_dot_f32_SOURCES         = one.c
bench_p_exp_f32_SOURCES         = one.c
bench_p_ftoi_SOURCES            = p_ftoi.c
bench_p_invcbrt_f32_SOURCES     = one.c
bench_p_inv_f32_SOURCES         = one.c
bench_p_invsqrt_f32_SOURCES     = one.c
bench_p_itof_SOURCES            = p_itof.c
bench_p_ln_f32_SOURCES          = one.c
bench_p_log10_f32_SOURCES       = one.c
bench_p_mac_f32_SOURCES         = one.c
bench_p_max_f32_SOURCES         = p_max_f32.c
bench_p_mean_f32_SOURCES        = one.c
bench_p_median_f32_SOURCES      = one.c
bench_p_min_f32_SOURCES         = p_min_f32.c
bench_p_mode_f32_SOURCES        = one.c
bench_p_mul_f32_SOURCES         = one.c
bench_p_popcount_u32_SOURCES    = p_popcount_u32.c
bench_p_popcount_u64_SOURCES    = p_popcount_u64.c
bench_p_pow_f32_SOURCES         = one.c
bench_p_rand_SOURCES            = p_rand.c
bench_p_sin_f32_SOURCES         = one.c
bench_p_sincos_f32_SOURCES      = p_sincos_f32.c
bench_p_sinh_f32_SOURCES        = one.c
bench_p_sort_f32_SOURCES        = one.c
bench_p_sqrt_f32_SOURCES        = one.c
bench_p_sub_f32_SOURCES         = one.c
bench_p_sum_f32_SOURCES         = one.c
bench_p_sumsq_f32_SOURCES       = one.c
bench_p_tan_f32_SOURCES         = one.c
bench_p_tanh_f32_SOURCES        = one.c

bench_p_absdiff_f32_CFLAGS      = -DFUNCTION=p_absdiff_f32  -DIS_BINARY
bench_p_abs_f32_CFLAGS          = -DFUNCTION=p_abs_f32      -DIS_UNARY
bench_p_acos_f32_CFLAGS         = -DFUNCTION=p_acos_f32     -DIS_UNARY
bench_p_acosh_f32_CFLAGS        = -DFUNCTION=p_acosh_f32    -DIS_UNARY
bench_p_add_f32_CFLAGS          = -DFUNCTION=p_add_f32      -DIS_BINARY
bench_p_asin_f32_CFLAGS         = -DFUNCTION=p_asin_f32     -DIS_UNARY
bench_p_asinh_f32_CFLAGS        = -DFUNCTION=p_asinh_f32    -DIS_UNARY
bench_p_atan2_f32_CFLAGS        = -DFUNCTION=p_atan2_f32    -DIS_BINARY
bench_p_atan_f32_CFLAGS         = -DFUNCTION=p_atan_f32     -DIS_UNARY
bench_p_atanh_f32_CFLAGS        = -DFUNCTION=p_atanh_f32    -DIS_UNARY
bench_p_cbrt_f32_CFLAGS         = -DFUNCTION=p_cbrt_f32     -DIS_UNARY
bench_p_cos_f32_CFLAGS          = -DFUNCTION=p_cos_f32      -DIS_UNARY
bench_p_cosh_f32_CFLAGS         = -DFUNCTION=p_cosh_f32     -DIS_UNARY
bench_p_div_f32_CFLAGS          = -DFUNCTION=p_div_f32      -DIS_BINARY
bench_p_dot_f32_CFLAGS          = -DFUNCTION=p_dot_f32      -DIS_BINARY
bench_p_exp_f32_CFLAGS          = -DFUNCTION=p_exp_f32      -DIS_UNARY
bench_p_ftoi_CFLAGS             = -DFUNCTION=p_ftoi         -DIS_UNARY
bench_p_invcbrt_f32_CFLAGS      = -DFUNCTION=p_invcbrt_f32  -DIS_UNARY
bench_p_inv_f32_CFLAGS          = -DFUNCTION=p_inv_f32      -DIS_UNARY
bench_p_invsqrt_f32_CFLAGS      = -DFUNCTION=p_invsqrt_f32  -DIS_UNARY
bench_p_itof_CFLAGS             = -DFUNCTION=p_itof         -DIS_UNARY
bench_p_ln_f32_CFLAGS           = -DFUNCTION=p_ln_f32       -DIS_UNARY
bench_p_log10_f32_CFLAGS        = -DFUNCTION=p_log10_f32    -DIS_UNARY
bench_p_mac_f32_CFLAGS          = -DFUNCTION=p_mac_f32      -DIS_BINARY
bench_p_max_f32_CFLAGS          = -DFUNCTION=p_max_f32      -DIS_UNARY
bench_p_mean_f32_CFLAGS         = -DFUNCTION=p_mean_f32     -DIS_UNARY
bench_p_median_f32_CFLAGS       = -DFUNCTION=p_median_f32   -DIS_UNARY
bench_p_min_f32_CFLAGS          = -DFUNCTION=p_min_f32      -DIS_UNARY
bench_p_mode_f32_CFLAGS         = -DFUNCTION=p_mode_f32     -DIS_UNARY
bench_p_mul_f32_CFLAGS          = -DFUNCTION=p_mul_f32      -DIS_BINARY
bench_p_popcount_u32_CFLAGS     = -DFUNCTION=p_popcount_u32 -DIS_UNARY
bench_p_popcount_u64_CFLAGS     = -DFUNCTION=p_popcount_u64 -DIS_UNARY
bench_p_pow_f32_CFLAGS          = -DFUNCTION=p_pow_f32      -DIS_BINARY
bench_p_rand_CFLAGS             = -DFUNCTION=p_rand         -DIS_UNARY
bench_p_sin_f32_CFLAGS          = -DFUNCTION=p_sin_f32      -DIS_UNARY
bench_p_sincos_f32_CFLAGS       = -DFUNCTION=p_sincos_f32   -DIS_UNARY
bench_p_sinh_f32_CFLAGS         = -DFUNCTION=p_sinh_f32     -DIS_UNARY
bench_p_sort_f32_CFLAGS         = -DFUNCTION=p_sort_f32     -DIS_UNARY
bench_p_sqrt_f32_CFLAGS         = -DFUNCTION=p_sqrt_f32     -DIS_UNARY
bench_p_sub_f32_CFLAGS          = -DFUNCTION=p_sub_f32      -DIS_BINARY
bench_p_sum_f32_CFLAGS          = -DFUNCTION=p_sum_f32      -DIS_UNARY
bench_p_sumsq_f32_CFLAGS        = -DFUNCTION=p_sumsq_f32    -DIS_UNARY
bench_p_tan_f32_CFLAGS          = -DFUNCTION=p_tan_f32      -DIS_UNARY
bench_p_tanh_f32_CFLAGS         = -DFUNCTION=p_tanh_f32     -DIS_UNARY

#bench_all_SOURCES = all.c
