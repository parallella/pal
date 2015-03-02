#!/bin/bash

set -e

./test_main.elf -f p_add_f32 -i p_add_f32.dat 
./test_main.elf -f p_mul_f32 -i p_mul_f32.dat 
./test_main.elf -f p_sub_f32 -i p_sub_f32.dat
 
