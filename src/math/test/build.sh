#!/bin/bash

set -e

gcc -o $1.elf -I../../include test_$1.c ../$1.c