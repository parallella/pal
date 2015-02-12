AUTOMAKE_OPTIONS = foreign

LDADD = ../libpal.la

noinst_PROGRAMS = hello_task simple_example sine_task

hello_task_SOURCES = hello_task.c
simple_example_SOURCES = simple_example.c

sine_task_SOURCES = sine_task.c ../../math/p_sin.c
sine_task_LDADD = -lm
