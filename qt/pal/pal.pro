#-------------------------------------------------
#
# Project created by QtCreator 2015-08-31T10:59:14
#
#-------------------------------------------------

QT       -= core gui

TARGET = pal
TEMPLATE = lib

INCLUDEPATH += ../../include

DEFINES += PAL_LIBRARY

SOURCES += \
    ../../src/base/p_append.c \
    ../../src/base/p_atomic_add.c \
    ../../src/base/p_atomic_and.c \
    ../../src/base/p_atomic_compswap.c \
    ../../src/base/p_atomic_or.c \
    ../../src/base/p_atomic_sub.c \
    ../../src/base/p_atomic_swap.c \
    ../../src/base/p_atomic_xor.c \
    ../../src/base/p_barrier.c \
    ../../src/base/p_broadcast.c \
    ../../src/base/p_close.c \
    ../../src/base/p_fence.c \
    ../../src/base/p_finalize.c \
    ../../src/base/p_free.c \
    ../../src/base/p_gather.c \
    ../../src/base/p_getaddr.c \
    ../../src/base/p_get_err.c \
    ../../src/base/p_getsymbol.c \
    ../../src/base/p_init.c \
    ../../src/base/p_load.c \
    ../../src/base/p_malloc.c \
    ../../src/base/p_memcpy.c \
    ../../src/base/p_mutex_init.c \
    ../../src/base/p_mutex_lock.c \
    ../../src/base/p_mutex_trylock.c \
    ../../src/base/p_mutex_unlock.c \
    ../../src/base/p_open.c \
    ../../src/base/p_query.c \
    ../../src/base/p_read.c \
    ../../src/base/p_remove.c \
    ../../src/base/p_rmalloc.c \
    ../../src/base/p_run.c \
    ../../src/base/p_scatter.c \
    ../../src/base/p_wait.c \
    ../../src/base/p_write.c \
    ../../src/dsp/p_acorr.c \
    ../../src/dsp/p_conv.c \
    ../../src/dsp/p_fir.c \
    ../../src/dsp/p_firdec.c \
    ../../src/dsp/p_firint.c \
    ../../src/dsp/p_firsym.c \
    ../../src/dsp/p_iir.c \
    ../../src/dsp/p_xcorr.c \
    ../../src/math/p_abs.c \
    ../../src/math/p_absdiff.c \
    ../../src/math/p_acos.c \
    ../../src/math/p_acosh.c \
    ../../src/math/p_add.c \
    ../../src/math/p_a_inv.c \
    ../../src/math/p_asin.c \
    ../../src/math/p_asinh.c \
    ../../src/math/p_atan.c \
    ../../src/math/p_atan2.c \
    ../../src/math/p_atanh.c \
    ../../src/math/p_cbrt.c \
    ../../src/math/p_cos.c \
    ../../src/math/p_cosh.c \
    ../../src/math/p_div.c \
    ../../src/math/p_dot.c \
    ../../src/math/p_exp.c \
    ../../src/math/p_ftoi.c \
    ../../src/math/p_inv.c \
    ../../src/math/p_invcbrt.c \
    ../../src/math/p_invsqrt.c \
    ../../src/math/p_itof.c \
    ../../src/math/p_ln.c \
    ../../src/math/p_log10.c \
    ../../src/math/p_mac.c \
    ../../src/math/p_max.c \
    ../../src/math/p_mean.c \
    ../../src/math/p_median.c \
    ../../src/math/p_min.c \
    ../../src/math/p_mode.c \
    ../../src/math/p_mul.c \
    ../../src/math/p_popcount.c \
    ../../src/math/p_pow.c \
    ../../src/math/p_rand.c \
    ../../src/math/p_sin.c \
    ../../src/math/p_sincos.c \
    ../../src/math/p_sinh.c \
    ../../src/math/p_sort.c \
    ../../src/math/p_sqrt.c \
    ../../src/math/p_stddev.c \
    ../../src/math/p_sub.c \
    ../../src/math/p_sum.c \
    ../../src/math/p_sumsq.c \
    ../../src/math/p_tan.c \
    ../../src/math/p_tanh.c \
    ../../src/image/p_box3x3.c \
    ../../src/image/p_conv2d.c \
    ../../src/image/p_gauss3x3.c \
    ../../src/image/p_grayscale.c \
    ../../src/image/p_harris3x3.c \
    ../../src/image/p_laplace3x3.c \
    ../../src/image/p_median3x3.c \
    ../../src/image/p_prewitt3x3.c \
    ../../src/image/p_sad8x8.c \
    ../../src/image/p_sad16x16.c \
    ../../src/image/p_scharr3x3.c \
    ../../src/image/p_sobel3x3.c \
    ../../src/fft/p_cfft.c

HEADERS +=\
    ../../include/pal.h \
    ../../include/pal_base.h \
    ../../include/pal_dsp.h \
    ../../include/pal_fft.h \
    ../../include/pal_image.h \
    ../../include/pal_math.h \
    ../../src/math/p_asin.h \
    ../../src/math/p_exp.h \
    ../../src/math/p_sqrt.h


unix {
    target.path = /usr/lib
    INSTALLS += target
}
