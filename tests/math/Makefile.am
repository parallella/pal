EXTRA_DIST = gold README

LIBS = -lm

if HOST_IS_EPIPHANY
    SIMPLE = notest.c
else
    AM_CPPFLAGS += @CHECK_CFLAGS@
    LIBS += @CHECK_LIBS@
    SIMPLE = simple.c simple.h
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
    gold/p_sincos_f32.gold.h \
    gold/p_sinh_f32.gold.h \
    gold/p_sort_f32.gold.h \
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
	@echo "struct gold builtin_gold[] = {" >> $@
	@sed -e 's/,/f, /g;s/^/    { /g;s/$$/f },/g;s/0\.00*f/0.0f/g;$$s/,$$//g' $< >> $@
	@echo "};" >> $@

clean-local:
	-rm -f gold/*.gold.h *.res


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
check_p_abs_f32_SOURCES         = $(SIMPLE) p_abs.c
check_p_absdiff_f32_SOURCES     = $(SIMPLE)
check_p_acos_f32_SOURCES        = $(SIMPLE) p_acos.c
check_p_acosh_f32_SOURCES       = $(SIMPLE) p_acosh.c
check_p_add_f32_SOURCES         = $(SIMPLE)
check_p_asin_f32_SOURCES        = $(SIMPLE) p_asin.c
check_p_asinh_f32_SOURCES       = $(SIMPLE) p_asinh.c
check_p_atan2_f32_SOURCES       = $(SIMPLE) p_atan2.c
check_p_atan_f32_SOURCES        = $(SIMPLE) p_atan.c
check_p_atanh_f32_SOURCES       = $(SIMPLE) p_atanh.c
check_p_cbrt_f32_SOURCES        = $(SIMPLE) p_cbrt.c
check_p_cos_f32_SOURCES         = $(SIMPLE) p_cos.c
check_p_cosh_f32_SOURCES        = $(SIMPLE) p_cosh.c
check_p_div_f32_SOURCES         = $(SIMPLE)
check_p_dot_f32_SOURCES         = $(SIMPLE)
check_p_exp_f32_SOURCES         = $(SIMPLE) p_exp.c
check_p_ftoi_SOURCES            = notest.c
check_p_inv_f32_SOURCES         = $(SIMPLE)
check_p_invcbrt_f32_SOURCES     = $(SIMPLE)
check_p_invsqrt_f32_SOURCES     = $(SIMPLE) p_invsqrt.c
check_p_itof_SOURCES            = notest.c
check_p_ln_f32_SOURCES          = $(SIMPLE) p_ln.c
check_p_log10_f32_SOURCES       = $(SIMPLE) p_log10.c
check_p_mac_f32_SOURCES         = $(SIMPLE)
check_p_max_f32_SOURCES         = notest.c
check_p_mean_f32_SOURCES        = $(SIMPLE)
check_p_median_f32_SOURCES      = $(SIMPLE)
check_p_min_f32_SOURCES         = notest.c
check_p_mode_f32_SOURCES        = notest.c
check_p_mul_f32_SOURCES         = $(SIMPLE)
check_p_popcount_SOURCES        = notest.c
check_p_pow_f32_SOURCES         = $(SIMPLE) p_pow.c
check_p_rand_SOURCES            = notest.c
check_p_sin_f32_SOURCES         = $(SIMPLE) p_sin.c
check_p_sincos_f32_SOURCES      = check_p_sincos.c
check_p_sinh_f32_SOURCES        = $(SIMPLE) p_sinh.c
check_p_sort_f32_SOURCES        = $(SIMPLE) p_sort.c
check_p_sqrt_f32_SOURCES        = $(SIMPLE) p_sqrt.c
check_p_sub_f32_SOURCES         = $(SIMPLE)
check_p_sum_f32_SOURCES         = $(SIMPLE)
check_p_sumsq_f32_SOURCES       = $(SIMPLE)
check_p_tan_f32_SOURCES         = $(SIMPLE) p_tan.c
check_p_tanh_f32_SOURCES        = $(SIMPLE) p_tanh.c

# In alphabetic order
# TODO: Mark all scalar output functions with -DSCALAR_OUTPUT
check_p_absdiff_f32_CFLAGS      = -DFUNCTION=p_absdiff_f32 -DIS_BINARY
check_p_abs_f32_CFLAGS          = -DFUNCTION=p_abs_f32     -DIS_UNARY
check_p_acos_f32_CFLAGS         = -DFUNCTION=p_acos_f32    -DIS_UNARY
check_p_acosh_f32_CFLAGS        = -DFUNCTION=p_acosh_f32   -DIS_UNARY
check_p_add_f32_CFLAGS          = -DFUNCTION=p_add_f32     -DIS_BINARY
check_p_asin_f32_CFLAGS         = -DFUNCTION=p_asin_f32    -DIS_UNARY
check_p_asinh_f32_CFLAGS        = -DFUNCTION=p_asinh_f32   -DIS_UNARY
check_p_atan2_f32_CFLAGS        = -DFUNCTION=p_atan2_f32   -DIS_BINARY
check_p_atan_f32_CFLAGS         = -DFUNCTION=p_atan_f32    -DIS_UNARY
check_p_atanh_f32_CFLAGS        = -DFUNCTION=p_atanh_f32   -DIS_UNARY
check_p_cbrt_f32_CFLAGS         = -DFUNCTION=p_cbrt_f32    -DIS_UNARY
check_p_cos_f32_CFLAGS          = -DFUNCTION=p_cos_f32     -DIS_UNARY
check_p_cosh_f32_CFLAGS         = -DFUNCTION=p_cosh_f32    -DIS_UNARY
check_p_div_f32_CFLAGS          = -DFUNCTION=p_div_f32     -DIS_BINARY
check_p_dot_f32_CFLAGS          = -DFUNCTION=p_dot_f32     -DIS_BINARY -DSCALAR_OUTPUT
check_p_exp_f32_CFLAGS          = -DFUNCTION=p_exp_f32     -DIS_UNARY
check_p_ftoi_CFLAGS             = -DFUNCTION=p_ftoi
check_p_invcbrt_f32_CFLAGS      = -DFUNCTION=p_invcbrt_f32 -DIS_UNARY
check_p_inv_f32_CFLAGS          = -DFUNCTION=p_inv_f32     -DIS_UNARY
check_p_invsqrt_f32_CFLAGS      = -DFUNCTION=p_invsqrt_f32 -DIS_UNARY
check_p_itof_CFLAGS             = -DFUNCTION=p_itof
check_p_ln_f32_CFLAGS           = -DFUNCTION=p_ln_f32      -DIS_UNARY
check_p_log10_f32_CFLAGS        = -DFUNCTION=p_log10_f32   -DIS_UNARY
check_p_mac_f32_CFLAGS          = -DFUNCTION=p_mac_f32     -DIS_BINARY
check_p_max_f32_CFLAGS          = -DFUNCTION=p_max_f32                 -DSCALAR_OUTPUT
check_p_mean_f32_CFLAGS         = -DFUNCTION=p_mean_f32    -DIS_UNARY  -DSCALAR_OUTPUT
check_p_median_f32_CFLAGS       = -DFUNCTION=p_median_f32  -DIS_UNARY  -DSCALAR_OUTPUT
check_p_min_f32_CFLAGS          = -DFUNCTION=p_min_f32                 -DSCALAR_OUTPUT
check_p_mode_f32_CFLAGS         = -DFUNCTION=p_mode_f32                -DSCALAR_OUTPUT
check_p_mul_f32_CFLAGS          = -DFUNCTION=p_mul_f32     -DIS_BINARY
check_p_popcount_CFLAGS         = -DFUNCTION=p_popcount
check_p_pow_f32_CFLAGS          = -DFUNCTION=p_pow_f32     -DIS_BINARY
check_p_rand_CFLAGS             = -DFUNCTION=p_rand                    -DSCALAR_OUTPUT
check_p_sin_f32_CFLAGS          = -DFUNCTION=p_sin_f32     -DIS_UNARY
check_p_sincos_f32_CFLAGS       = -DFUNCTION=p_sincos_f32
check_p_sinh_f32_CFLAGS         = -DFUNCTION=p_sinh_f32    -DIS_UNARY
check_p_sort_f32_CFLAGS         = -DFUNCTION=p_sort_f32    -DIS_UNARY
check_p_sqrt_f32_CFLAGS         = -DFUNCTION=p_sqrt_f32    -DIS_UNARY
check_p_sub_f32_CFLAGS          = -DFUNCTION=p_sub_f32     -DIS_BINARY
check_p_sum_f32_CFLAGS          = -DFUNCTION=p_sum_f32     -DIS_UNARY  -DSCALAR_OUTPUT
check_p_sumsq_f32_CFLAGS        = -DFUNCTION=p_sumsq_f32   -DIS_UNARY  -DSCALAR_OUTPUT
check_p_tan_f32_CFLAGS          = -DFUNCTION=p_tan_f32     -DIS_UNARY
check_p_tanh_f32_CFLAGS         = -DFUNCTION=p_tanh_f32    -DIS_UNARY

TESTS = $(check_PROGRAMS)

# Ubuntu 14.04 has automake-1.11. Need automake-1.12 for nicer test features.
# This will have to do for now.
TESTS_ENVIRONMENT = ../runtest
