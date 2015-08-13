AUTOMAKE_OPTIONS = foreign

SUBDIRS = image math

EXTRA_DIST = \
    runbench.default.c \
    runbench.noimpl.c \
    runbench.epiphany.c \
    bench-all.sh.in

CLEANFILES = runbench

# TODO: Move to configure.ac.
if HOST_IS_EPIPHANY
ESDK              = /opt/adapteva/esdk
EINCS             = -I$(ESDK)/tools/host/include
ELIBS             = -L$(ESDK)/tools/host/lib
CFLAGS_FOR_BUILD  += $(EINCS)
LDFLAGS_FOR_BUILD += $(ELIBS) -le-hal -le-loader
RUNBENCH_SRC      = runbench.epiphany.c
else
RUNBENCH_SRC      = runbench.default.c
BENCHMARK_LIBS    += -lm
endif

BUILT_SOURCES = runbench$(BUILD_EXEEXT) libbenchmark.la

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


runbench$(EXEEXT): $(RUNBENCH_SRC)
	$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) $< -o $@ $(LDFLAGS_FOR_BUILD)

all-local: runbench$(EXEEXT)
	chmod +x bench-all.sh
