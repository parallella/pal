#!/bin/bash

set -e

gcc -I../../../include simple_example.c \
../p_init.c \
../p_query.c \
../p_load.c \
../p_open.c \
../p_malloc.c \
../p_write.c \
../p_read.c \
../p_barrier.c \
../p_close.c \
../p_run.c \
../p_free.c \
../p_finalize.c \
-o simple_example.elf

#Task compilation
gcc hello_task.c -o hello_task.elf
gcc -I../../../include sine_task.c ../../math/p_sin.c -o sine_task.elf -lm


