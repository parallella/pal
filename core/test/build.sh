#!/bin/bash

gcc simple_example.c \
../p_init.c \
../p_query.c \
../p_load.c \
../p_open.c \
../p_malloc.c \
../p_write.c \
../p_read.c \
../p_close.c \
../p_run.c \
../p_free.c \
../p_finalize.c \
-o simple_example.elf -I/home/aolofsson/Work_all/Work_december/pal/include

