AUTOMAKE_OPTIONS = foreign


noinst_LTLIBRARIES = libbenchmark.la
libbenchmark_la_SOURCES = \
    benchmark.c
libbenchmark_la_CFLAGS = -DNDEBUG

noinst_HEADERS = benchmark.h bench_tmpl.h
noinst_PROGRAMS = example

example_SOURCES = example.c
example_CFLAGS = -DNDEBUG
example_LDADD = libbenchmark.la $(top_builddir)/src/libpal.la
