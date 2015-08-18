AUTOMAKE_OPTIONS = foreign

LDADD = -lm $(top_builddir)/src/libpal.la


if HOST_IS_EPIPHANY
noinst_PROGRAMS = harris # filters won't compile due to compiler bug.
else
noinst_PROGRAMS = filters harris
endif

filters_SOURCES = filters.c stb_image.h stb_image_write.h
filters_CPPFLAGS = $(AM_CPPFLAGS) -DABS_TOP_SRCDIR=\"$(abs_top_srcdir)\"


harris_SOURCES = harris.c
if HOST_IS_EPIPHANY
harris_LDFLAGS = $(EPIPHANY_FAST_LDFLAGS)
endif
