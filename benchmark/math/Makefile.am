AUTOMAKE_OPTIONS = foreign

noinst_PROGRAMS = bench_all

bench_all_SOURCES = all.c
bench_all_CFLAGS = -DNDEBUG
bench_all_LDADD = $(top_builddir)/benchmark/libbenchmark.la $(top_builddir)/src/libpal.la
