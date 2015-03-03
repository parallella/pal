#!/bin/bash

set -e

ESDK=${EPIPHANY_HOME}
ELIBS=${ESDK}/tools/host/lib
EINCS=${ESDK}/tools/host/include
ELDF=${ESDK}/bsps/current/fast.ldf

SCRIPT=$(readlink -f "$0")
EXEPATH=$(dirname "$SCRIPT")
cd $EXEPATH

CROSS_PREFIX=
case $(uname -p) in
	arm*)
		# Use native arm compiler (no cross prefix)
		CROSS_PREFIX=
		;;
	   *)
		# Use cross compiler
		CROSS_PREFIX="arm-linux-gnueabihf-"
		;;
esac

# Build HOST side application
${CROSS_PREFIX}gcc src/hello_world.c -o Debug/hello_world.elf -I ${EINCS} -L ${ELIBS} -le-hal #-le-loader

# Build DEVICE side program
e-gcc -T ${ELDF} src/e_hello_world.c -o Debug/e_hello_world.elf -le-lib

# Convert ebinary to SREC file
e-objcopy --srec-forceS3 --output-target srec Debug/e_hello_world.elf Debug/e_hello_world.srec

# Build Erlang program
erlc -oDebug src/hello_world.erl
cp src/runerl.escript Debug
