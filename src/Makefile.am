SUBDIRS = base dsp fft image math
# TODO:
# Add blas and wrappers to SUBDIRS

EXTRA_DIST = blas wrappers

lib_LTLIBRARIES = libpal.la
libpal_la_SOURCES = dummy.c
libpal_la_LIBADD = \
    base/libpal-base.la \
    dsp/libpal-dsp.la \
    fft/libpal-fft.la \
    image/libpal-image.la \
    math/libpal-math.la

#TODO:
#    blas/libpal-blas.la \
#

# Only public API functions should be visible
libpal_la_LDFLAGS = -export-symbols-regex '^p_|^__pal_'
