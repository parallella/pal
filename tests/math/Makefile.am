EXTRA_DIST = gold README

LIBS = -lm

if HOST_IS_EPIPHANY
    SIMPLE = notest.c
else
    AM_CPPFLAGS += @CHECK_CFLAGS@
    LIBS += @CHECK_LIBS@
    SIMPLE = simple.c
endif

LDADD = $(top_builddir)/src/libpal.la

# TODO: For cross compilation: Build for build arch, not host arch.
noinst_PROGRAMS = rangen
rangen_SOURCES = rangen.c

BUILT_SOURCES = \
    gold/p_absdiff_f32.gold.h \
    gold/p_abs_f32.gold.h \
    gold/p_acos_f32.gold.h \
    gold/p_acosh_f32.gold.h \
    gold/p_add_f32.gold.h \
    gold/p_asin_f32.gold.h \
    gold/p_asinh_f32.gold.h \
    gold/p_atan2_f32.gold.h \
    gold/p_atan_f32.gold.h \
    gold/p_atanh_f32.gold.h \
    gold/p_cbrt_f32.gold.h \
    gold/p_cos_f32.gold.h \
    gold/p_cosh_f32.gold.h \
    gold/p_div_f32.gold.h \
    gold/p_dot_f32.gold.h \
    gold/p_exp_f32.gold.h \
    gold/p_invcbrt_f32.gold.h \
    gold/p_inv_f32.gold.h \
    gold/p_invsqrt_f32.gold.h \
    gold/p_ln_f32.gold.h \
    gold/p_log10_f32.gold.h \
    gold/p_mac_f32.gold.h \
    gold/p_mean_f32.gold.h \
    gold/p_median_f32.gold.h \
    gold/p_mul_f32.gold.h \
    gold/p_pow_f32.gold.h \
    gold/p_sin_f32.gold.h \
    gold/p_sinh_f32.gold.h \
    gold/p_sqrt_f32.gold.h \
    gold/p_sub_f32.gold.h \
    gold/p_sum_f32.gold.h \
    gold/p_sumsq_f32.gold.h \
    gold/p_tan_f32.gold.h \
    gold/p_tanh_f32.gold.h

SUFFIXES = .dat .gold.h

# CSV to C struct
.dat.gold.h:
	@$(MKDIR_P) gold
	@echo "$< --> $@"
	@echo "struct gold gold[] = {" >> $@
	@sed -e 's/,/f, /g;s/^/    { /g;s/$$/f },/g;s/0\.00*f/0.0f/g;$$s/,$$//g' $< >> $@
	@echo "};" >> $@

clean-local:
	-rm -f gold/*.gold.h


# In alphabetic order
check_PROGRAMS = \
	check_p_absdiff_f32 \
	check_p_abs_f32 \
	check_p_acos_f32 \
	check_p_acosh_f32 \
	check_p_add_f32 \
	check_p_asin_f32 \
	check_p_asinh_f32 \
	check_p_atan2_f32 \
	check_p_atan_f32 \
	check_p_atanh_f32 \
	check_p_cbrt_f32 \
	check_p_cos_f32 \
	check_p_cosh_f32 \
	check_p_div_f32 \
	check_p_dot_f32 \
	check_p_exp_f32 \
	check_p_ftoi \
	check_p_invcbrt_f32 \
	check_p_inv_f32 \
	check_p_invsqrt_f32 \
	check_p_itof \
	check_p_ln_f32 \
	check_p_log10_f32 \
	check_p_mac_f32 \
	check_p_max_f32 \
	check_p_mean_f32 \
	check_p_median_f32 \
	check_p_min_f32 \
	check_p_mode_f32 \
	check_p_mul_f32 \
	check_p_popcount \
	check_p_pow_f32 \
	check_p_rand \
	check_p_sin_f32 \
	check_p_sincos_f32 \
	check_p_sinh_f32 \
	check_p_sort_f32 \
	check_p_sqrt_f32 \
	check_p_sub_f32 \
	check_p_sum_f32 \
	check_p_sumsq_f32 \
	check_p_tan_f32 \
	check_p_tanh_f32

# In alphabetic order
check_p_abs_f32_SOURCES         = $(SIMPLE)
check_p_absdiff_f32_SOURCES     = $(SIMPLE)
check_p_acos_f32_SOURCES        = $(SIMPLE)
check_p_acosh_f32_SOURCES       = $(SIMPLE)
check_p_add_f32_SOURCES         = $(SIMPLE)
check_p_asin_f32_SOURCES        = $(SIMPLE)
check_p_asinh_f32_SOURCES       = $(SIMPLE)
check_p_atan2_f32_SOURCES       = $(SIMPLE)
check_p_atan_f32_SOURCES        = $(SIMPLE)
check_p_atanh_f32_SOURCES       = $(SIMPLE)
check_p_cbrt_f32_SOURCES        = $(SIMPLE)
check_p_cos_f32_SOURCES         = $(SIMPLE)
check_p_cosh_f32_SOURCES        = $(SIMPLE)
check_p_div_f32_SOURCES         = $(SIMPLE)
check_p_dot_f32_SOURCES         = $(SIMPLE)
check_p_exp_f32_SOURCES         = $(SIMPLE)
check_p_ftoi_SOURCES            = notest.c
check_p_inv_f32_SOURCES         = $(SIMPLE)
check_p_invcbrt_f32_SOURCES     = $(SIMPLE)
check_p_invsqrt_f32_SOURCES     = $(SIMPLE)
check_p_itof_SOURCES            = notest.c
check_p_ln_f32_SOURCES          = $(SIMPLE)
check_p_log10_f32_SOURCES       = $(SIMPLE)
check_p_mac_f32_SOURCES         = $(SIMPLE)
check_p_max_f32_SOURCES         = notest.c
check_p_mean_f32_SOURCES        = $(SIMPLE)
check_p_median_f32_SOURCES      = $(SIMPLE)
check_p_min_f32_SOURCES         = notest.c
check_p_mode_f32_SOURCES        = notest.c
check_p_mul_f32_SOURCES         = $(SIMPLE)
check_p_popcount_SOURCES        = notest.c
check_p_pow_f32_SOURCES         = $(SIMPLE)
check_p_rand_SOURCES            = notest.c
check_p_sin_f32_SOURCES         = $(SIMPLE)
check_p_sincos_f32_SOURCES      = notest.c
check_p_sinh_f32_SOURCES        = $(SIMPLE)
check_p_sort_f32_SOURCES        = notest.c
check_p_sqrt_f32_SOURCES        = $(SIMPLE)
check_p_sub_f32_SOURCES         = $(SIMPLE)
check_p_sum_f32_SOURCES         = $(SIMPLE)
check_p_sumsq_f32_SOURCES       = $(SIMPLE)
check_p_tan_f32_SOURCES         = $(SIMPLE)
check_p_tanh_f32_SOURCES        = $(SIMPLE)

# In alphabetic order
check_p_absdiff_f32_CFLAGS      = -DTEST_FUNCTION=p_absdiff_f32 -DIS_BINARY
check_p_abs_f32_CFLAGS          = -DTEST_FUNCTION=p_abs_f32     -DIS_UNARY
check_p_acos_f32_CFLAGS         = -DTEST_FUNCTION=p_acos_f32    -DIS_UNARY
check_p_acosh_f32_CFLAGS        = -DTEST_FUNCTION=p_acosh_f32   -DIS_UNARY
check_p_add_f32_CFLAGS          = -DTEST_FUNCTION=p_add_f32     -DIS_BINARY
check_p_asin_f32_CFLAGS         = -DTEST_FUNCTION=p_asin_f32    -DIS_UNARY
check_p_asinh_f32_CFLAGS        = -DTEST_FUNCTION=p_asinh_f32   -DIS_UNARY
check_p_atan2_f32_CFLAGS        = -DTEST_FUNCTION=p_atan2_f32   -DIS_BINARY
check_p_atan_f32_CFLAGS         = -DTEST_FUNCTION=p_atan_f32    -DIS_UNARY
check_p_atanh_f32_CFLAGS        = -DTEST_FUNCTION=p_atanh_f32   -DIS_UNARY
check_p_cbrt_f32_CFLAGS         = -DTEST_FUNCTION=p_cbrt_f32    -DIS_UNARY
check_p_cos_f32_CFLAGS          = -DTEST_FUNCTION=p_cos_f32     -DIS_UNARY
check_p_cosh_f32_CFLAGS         = -DTEST_FUNCTION=p_cosh_f32    -DIS_UNARY
check_p_div_f32_CFLAGS          = -DTEST_FUNCTION=p_div_f32     -DIS_BINARY
check_p_dot_f32_CFLAGS          = -DTEST_FUNCTION=p_dot_f32     -DIS_BINARY
check_p_exp_f32_CFLAGS          = -DTEST_FUNCTION=p_exp_f32     -DIS_UNARY
check_p_ftoi_CFLAGS             = -DTEST_FUNCTION=p_ftoi
check_p_invcbrt_f32_CFLAGS      = -DTEST_FUNCTION=p_invcbrt_f32 -DIS_UNARY
check_p_inv_f32_CFLAGS          = -DTEST_FUNCTION=p_inv_f32     -DIS_UNARY
check_p_invsqrt_f32_CFLAGS      = -DTEST_FUNCTION=p_invsqrt_f32 -DIS_UNARY
check_p_itof_CFLAGS             = -DTEST_FUNCTION=p_itof
check_p_ln_f32_CFLAGS           = -DTEST_FUNCTION=p_ln_f32      -DIS_UNARY
check_p_log10_f32_CFLAGS        = -DTEST_FUNCTION=p_log10_f32   -DIS_UNARY
check_p_mac_f32_CFLAGS          = -DTEST_FUNCTION=p_mac_f32     -DIS_BINARY
check_p_max_f32_CFLAGS          = -DTEST_FUNCTION=p_max_f32
check_p_mean_f32_CFLAGS         = -DTEST_FUNCTION=p_mean_f32    -DIS_UNARY
check_p_median_f32_CFLAGS       = -DTEST_FUNCTION=p_median_f32  -DIS_UNARY
check_p_min_f32_CFLAGS          = -DTEST_FUNCTION=p_min_f32
check_p_mode_f32_CFLAGS         = -DTEST_FUNCTION=p_mode_f32
check_p_mul_f32_CFLAGS          = -DTEST_FUNCTION=p_mul_f32     -DIS_BINARY
check_p_popcount_CFLAGS         = -DTEST_FUNCTION=p_popcount
check_p_pow_f32_CFLAGS          = -DTEST_FUNCTION=p_pow_f32     -DIS_BINARY
check_p_rand_CFLAGS             = -DTEST_FUNCTION=p_rand
check_p_sin_f32_CFLAGS          = -DTEST_FUNCTION=p_sin_f32     -DIS_UNARY
check_p_sincos_f32_CFLAGS       = -DTEST_FUNCTION=p_sin_cos_f32
check_p_sinh_f32_CFLAGS         = -DTEST_FUNCTION=p_sinh_f32    -DIS_UNARY
check_p_sort_f32_CFLAGS         = -DTEST_FUNCTION=p_sort_f32
check_p_sqrt_f32_CFLAGS         = -DTEST_FUNCTION=p_sqrt_f32    -DIS_UNARY
check_p_sub_f32_CFLAGS          = -DTEST_FUNCTION=p_sub_f32     -DIS_BINARY
check_p_sum_f32_CFLAGS          = -DTEST_FUNCTION=p_sum_f32     -DIS_UNARY
check_p_sumsq_f32_CFLAGS        = -DTEST_FUNCTION=p_sumsq_f32   -DIS_UNARY
check_p_tan_f32_CFLAGS          = -DTEST_FUNCTION=p_tan_f32     -DIS_UNARY
check_p_tanh_f32_CFLAGS         = -DTEST_FUNCTION=p_tanh_f32    -DIS_UNARY

TESTS = $(check_PROGRAMS)

# Ubuntu 14.04 has automake-1.11. Need automake-1.12 for nicer test features.
# This will have to do for now.
TESTS_ENVIRONMENT = ../runtest


