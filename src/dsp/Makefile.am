# TODO: add host target conditional subdirs
SUBDIRS =
EXTRA_DIST = asm

noinst_LTLIBRARIES = libpal-dsp.la
libpal_dsp_la_SOURCES = \
    p_acorr.c \
    p_conv.c \
    p_fir.c \
    p_firdec.c \
    p_firint.c \
    p_firsym.c \
    p_iir.c \
    p_xcorr.c

libpal_dsp_la_LIBADD = -lm
libpal_dsp_la_CPPFLAGS = @AM_CPPFLAGS@ ${debug_CPPFLAGS}


