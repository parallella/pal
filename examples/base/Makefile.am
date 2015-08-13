AUTOMAKE_OPTIONS = foreign

LDADD = $(top_builddir)/src/libpal.la

noinst_PROGRAMS = hello_task simple_example sine_task sort_task median_task mode_task

hello_task_SOURCES = hello_task.c
if HOST_IS_EPIPHANY
hello_task_LDFLAGS     = $(EPIPHANY_FAST_LDFLAGS)
simple_example_LDFLAGS = $(EPIPHANY_FAST_LDFLAGS)
sort_task_LDFLAGS      = $(EPIPHANY_FAST_LDFLAGS)
median_task_LDFLAGS    = $(EPIPHANY_FAST_LDFLAGS)
mode_task_LDFLAGS      = $(EPIPHANY_FAST_LDFLAGS)
endif

simple_example_SOURCES = simple_example.c

sine_task_SOURCES = sine_task.c

sort_task_SOURCES = sort_task.c

median_task_SOURCES = median_task.c

mode_task_SOURCES = mode_task.c
