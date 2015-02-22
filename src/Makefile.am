SUBDIRS = base math
EXTRA_DIST = blas dsp fft image wrappers

lib_LTLIBRARIES = libpal.la
libpal_la_SOURCES = dummy.c
libpal_la_LIBADD = base/libpal-base.la math/libpal-math.la
# Only public API functions should be visible
libpal_la_LDFLAGS = -export-symbols-regex '^p_|^__pal_'
