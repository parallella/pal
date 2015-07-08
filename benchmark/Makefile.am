AUTOMAKE_OPTIONS = foreign

SUBDIRS = image math

EXTRA_DIST = \
    runbench.default.c \
    runbench.noimpl.c \
    runbench.epiphany.c \
    bench-all.sh.in

CLEANFILES = runbench

# TODO: Move to configure.ac. Use expansion in here instead.
#
BUILDCC = gcc
if HOST_IS_EPIPHANY
ESDK = /opt/adapteva/esdk
EINCS = -I$(ESDK)/tools/host/include
ELIBS = -L$(ESDK)/tools/host/lib
BUILDCFLAGS = $(EINCS)
BUILDLDFLAGS = $(ELIBS) -le-hal -le-loader
RUNBENCH_SRC = runbench.epiphany.c
else
BUILDCFLAGS = $(CFLAGS)
BUILDLDFLAGS = $(LDFLAGS)
RUNBENCH_SRC = runbench.default.c
endif

BUILT_SOURCES = runbench libbenchmark.la

noinst_LTLIBRARIES = libbenchmark.la
libbenchmark_la_SOURCES = \
	benchmark.c
libbenchmark_la_CPPFLAGS = $(AM_CPPFLAGS) -DNDEBUG
libbenchmark_la_LIBADD = $(BENCHMARK_LIBS)

noinst_HEADERS = benchmark.h bench_tmpl.h
noinst_PROGRAMS = example

example_SOURCES = example.c
example_CPPFLAGS = $(AM_CPPFLAGS) -DNDEBUG
example_LDADD = libbenchmark.la $(top_builddir)/src/libpal.la


runbench: $(RUNBENCH_SRC)
	$(BUILDCC) $(BUILDCFLAGS) $< -o $@ $(BUILDLDFLAGS)

all-local: runbench
	chmod +x bench-all.sh
