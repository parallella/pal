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
check_p_acorr_f32 \
check_p_conv_f32 \
check_p_fir_f32 \
check_p_firdec_f32 \
check_p_firint_f32 \
check_p_firsym_f32 \
check_p_iir_f32 \
check_p_xcorr_f32

# Alphabetic order
check_p_acorr_f32_SOURCES        = check_p_acorr_f32.c acorr_test_data.h
check_p_conv_f32_SOURCES         = $(NOTEST)
check_p_fir_f32_SOURCES          = check_p_fir_f32.c fir_test_data.h
check_p_firdec_f32_SOURCES       = $(NOTEST)
check_p_firint_f32_SOURCES       = $(NOTEST)
check_p_firsym_f32_SOURCES       = check_p_firsym_f32.c firsym_test_data.h
check_p_iir_f32_SOURCES          = $(NOTEST)
check_p_xcorr_f32_SOURCES        = check_p_xcorr_f32.c \
                                   xcorr_test_data_1.h \
                                   xcorr_test_data_2.h \
                                   xcorr_test_data_3.h

# Alphabetic order
check_p_acorr_f32_CFLAGS         = -DFUNCTION=p_acorr_f32
check_p_conv_f32_CFLAGS          = -DFUNCTION=p_conv_f32
check_p_fir_f32_CFLAGS           = -DFUNCTION=p_fir_f32
check_p_firdec_f32_CFLAGS        = -DFUNCTION=p_firdec_f32
check_p_firint_f32_CFLAGS        = -DFUNCTION=p_firint_f32
check_p_firsym_f32_CFLAGS        = -DFUNCTION=p_firsym_f32
check_p_iir_f32_CFLAGS           = -DFUNCTION=p_iir_f32
check_p_xcorr_f32_CFLAGS         = -DFUNCTION=p_xcorr_f32


# Alphabetic order
check_p_acorr_f32_LDFLAGS        = $(CHECKLDFLAGS)
check_p_conv_f32_LDFLAGS         = $(CHECKLDFLAGS)
check_p_fir_f32_LDFLAGS          = $(CHECKLDFLAGS)
check_p_firdec_f32_LDFLAGS       = $(CHECKLDFLAGS)
check_p_firint_f32_LDFLAGS       = $(CHECKLDFLAGS)
check_p_firsym_f32_LDFLAGS       = $(CHECKLDFLAGS)
check_p_iir_f32_LDFLAGS          = $(CHECKLDFLAGS)
check_p_xcorr_f32_LDFLAGS        = $(CHECKLDFLAGS)

TESTS = $(check_PROGRAMS)

# Ubuntu 14.04 has automake-1.11. Need automake-1.12 for nicer test features.
# This will have to do for now.
#TESTS_ENVIRONMENT = ../runtest
LOG_COMPILER = ../runtest
