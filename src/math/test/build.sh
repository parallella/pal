#!/bin/bash

set -e

gcc -o test_main.elf \
    test_main.c \
    ../p_add.c \
    ../p_mul.c \
    ../p_sub.c \
    -I../../../include

