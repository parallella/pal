AUTOMAKE_OPTIONS = foreign

LDADD = -lm $(top_builddir)/src/libpal.la

filters_SOURCES = filters.c stb_image.h stb_image_write.h
filters_CPPFLAGS = $(AM_CPPFLAGS) -DABS_TOP_SRCDIR=\"$(abs_top_srcdir)\"
harris_SOURCES = harris.c
filters2_host_SOURCES = filters2-host.c stb_image.h stb_image_write.h
filters2_dev_sources = filters2-dev.c

if HOST_IS_EPIPHANY
std_programs = harris # filters won't compile due to compiler bug.
harris_LDFLAGS = $(EPIPHANY_FAST_LDFLAGS)
else
std_programs = filters harris
endif

noinst_PROGRAMS = $(std_programs)

all-local:
	$(MKDIR_P) dataset
