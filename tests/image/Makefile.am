LIBS = -lm

AM_CPPFLAGS += -I$(top_srcdir)/tests
if HOST_IS_EPIPHANY
CHECKLDFLAGS=$(EPIPHANY_FAST_LDFLAGS)
NOTEST = notest.epiphany.c
else
CHECKLDFLAGS=$(AM_LDFLAGS)
NOTEST = notest.c
endif

SIMPLE = simple.c simple.h

LDADD = $(top_builddir)/src/libpal.la
LDADD += $(top_builddir)/tests/libutest.la

# Alphabetic order
check_PROGRAMS = \
check_p_box3x3_f32 \
check_p_conv2d_f32 \
check_p_gauss3x3_f32 \
check_p_harris3x3_f32 \
check_p_laplace3x3_f32 \
check_p_median3x3_f32 \
check_p_prewitt3x3_f32 \
check_p_rgb2grayscale_f32 \
check_p_sad16x16_f32 \
check_p_sad8x8_f32 \
check_p_scharr3x3_f32 \
check_p_sobel3x3_f32

# Alphabetic order
check_p_box3x3_f32_SOURCES         = check_p_box3x3_f32.c
check_p_conv2d_f32_SOURCES         = $(NOTEST)
check_p_gauss3x3_f32_SOURCES       = $(NOTEST)
check_p_harris3x3_f32_SOURCES      = $(NOTEST)
check_p_laplace3x3_f32_SOURCES     = $(NOTEST)
check_p_median3x3_f32_SOURCES      = $(NOTEST)
check_p_prewitt3x3_f32_SOURCES     = $(NOTEST)
check_p_rgb2grayscale_f32_SOURCES  = check_p_rgb2greyscale_f32.c \
                                     rgb2greyscale_test_data.h
check_p_sad16x16_f32_SOURCES       = $(NOTEST)
check_p_sad8x8_f32_SOURCES         = $(NOTEST)
check_p_scharr3x3_f32_SOURCES      = $(NOTEST)
check_p_sobel3x3_f32_SOURCES       = $(NOTEST)

# Alphabetic order
check_p_box3x3_f32_CFLAGS          = -DFUNCTION=p_box3x3_f32
check_p_conv2d_f32_CFLAGS          = -DFUNCTION=p_conv2d_f32
check_p_gauss3x3_f32_CFLAGS        = -DFUNCTION=p_gauss3x3_f32
check_p_harris3x3_f32_CFLAGS       = -DFUNCTION=p_harris3x3_f32
check_p_laplace3x3_f32_CFLAGS      = -DFUNCTION=p_laplace3x3_f32
check_p_median3x3_f32_CFLAGS       = -DFUNCTION=p_median3x3_f32
check_p_prewitt3x3_f32_CFLAGS      = -DFUNCTION=p_prewitt3x3_f32
check_p_rgb2grayscale_f32_CFLAGS   = -DFUNCTION=p_rgb2grayscale_f32
check_p_sad16x16_f32_CFLAGS        = -DFUNCTION=p_sad16x16_f32
check_p_sad8x8_f32_CFLAGS          = -DFUNCTION=p_sad8x8_f32
check_p_scharr3x3_f32_CFLAGS       = -DFUNCTION=p_scharr3x3_f32
check_p_sobel3x3_f32_CFLAGS        = -DFUNCTION=p_sobel3x3_f32

# Alphabetic order
check_p_box3x3_f32_LDFLAGS         = $(CHECKLDFLAGS)
check_p_conv2d_f32_LDFLAGS         = $(CHECKLDFLAGS)
check_p_gauss3x3_f32_LDFLAGS       = $(CHECKLDFLAGS)
check_p_harris3x3_f32_LDFLAGS      = $(CHECKLDFLAGS)
check_p_laplace3x3_f32_LDFLAGS     = $(CHECKLDFLAGS)
check_p_median3x3_f32_LDFLAGS      = $(CHECKLDFLAGS)
check_p_prewitt3x3_f32_LDFLAGS     = $(CHECKLDFLAGS)
check_p_rgb2grayscale_f32_LDFLAGS  = $(CHECKLDFLAGS)
check_p_sad16x16_f32_LDFLAGS       = $(CHECKLDFLAGS)
check_p_sad8x8_f32_LDFLAGS         = $(CHECKLDFLAGS)
check_p_scharr3x3_f32_LDFLAGS      = $(CHECKLDFLAGS)
check_p_sobel3x3_f32_LDFLAGS       = $(CHECKLDFLAGS)


TESTS = $(check_PROGRAMS)

# Ubuntu 14.04 has automake-1.11. Need automake-1.12 for nicer test features.
# This will have to do for now.
LOG_COMPILER = ../runtest
