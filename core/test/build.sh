#!/bin/bash

gcc simple_example.c \
../pal_init.c \
../pal_query.c \
../pal_load.c \
../pal_open.c \
../pal_malloc.c \
../pal_write.c \
../pal_read.c \
../pal_close.c \
../pal_run.c \
../pal_free.c \
../pal_finalize.c \
-o simple_example.elf -I/home/aolofsson/Work_all/Work_december/pal/include

