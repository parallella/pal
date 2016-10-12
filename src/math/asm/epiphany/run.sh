#!/bin/bash
function="p_add_f32"
e-gcc -Os -mlong-calls -mfp-mode=round-nearest main.c -T $EPIPHANY_HOME/bsps/current/internal.ldf $function.S -o main.elf
#e-as $function.S -o $function.o 
#e-ld --defsym=__stack=0x7ff0 -T $EPIPHANY_HOME/bsps/current/internal.ldf $function.o main.o -o main.elf

e-run --trace main.elf
