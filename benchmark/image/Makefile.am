AUTOMAKE_OPTIONS = foreign

LDADD = $(top_builddir)/benchmark/libbenchmark.la $(top_builddir)/src/libpal.la

# Can only do one bench at a time (size)
# In alphabetic order

# Not implemented
    #bench_p_conv2d_f32 \
    #bench_p_harris3x3_f32 \
    #bench_p_grayscale_f32 \
    #bench_p_sad16x16_f32 \
    #bench_p_sad8x8_f32

if HOST_IS_EPIPHANY
noinst_PROGRAMS = \
    bench_p_box3x3_f32 \
    bench_p_gauss3x3_f32 \
    bench_p_laplace3x3_f32 \
    bench_p_median3x3_f32 \
    bench_p_prewitt3x3_f32 \
    bench_p_scharr3x3_f32 \
    bench_p_sobel3x3_f32

else
noinst_PROGRAMS = bench_all
endif

bench_p_box3x3_f32_SOURCES            = one.c
#bench_p_conv2d_f32_SOURCES            = p_conv2d_f32.c
bench_p_gauss3x3_f32_SOURCES          = one.c
#bench_p_grayscale_f32_SOURCES         = one.c
#bench_p_harris3x3_f32_SOURCES         = one.c
bench_p_laplace3x3_f32_SOURCES        = one.c
bench_p_median3x3_f32_SOURCES         = one.c
bench_p_prewitt3x3_f32_SOURCES        = one.c
#bench_p_sad16x16_f32_SOURCES          = p_sad.c
#bench_p_sad8x8_f32_SOURCES            = p_sad.c
bench_p_scharr3x3_f32_SOURCES         = one.c
bench_p_sobel3x3_f32_SOURCES          = one.c

bench_p_box3x3_f32_CFLAGS             = -DFUNCTION=p_box3x3_f32
bench_p_conv2d_f32_CFLAGS             = -DFUNCTION=p_conv2d_f32
bench_p_gauss3x3_f32_CFLAGS           = -DFUNCTION=p_gauss3x3_f32
bench_p_grayscale_f32_CFLAGS          = -DFUNCTION=p_grayscale_f32
#bench_p_harris3x3_f32_CFLAGS          = -DFUNCTION=p_harris3x3_f32
bench_p_laplace3x3_f32_CFLAGS         = -DFUNCTION=p_laplace3x3_f32
bench_p_median3x3_f32_CFLAGS          = -DFUNCTION=p_median3x3_f32
bench_p_prewitt3x3_f32_CFLAGS         = -DFUNCTION=p_prewitt3x3_f32
bench_p_sad16x16_f32_CFLAGS           = -DFUNCTION=p_sad16x16_f32
bench_p_sad8x8_f32_CFLAGS             = -DFUNCTION=p_sad8x8_f32
bench_p_scharr3x3_f32_CFLAGS          = -DFUNCTION=p_scharr3x3_f32
bench_p_sobel3x3_f32_CFLAGS           = -DFUNCTION=p_sobel3x3_f32

bench_all_SOURCES = all.c
