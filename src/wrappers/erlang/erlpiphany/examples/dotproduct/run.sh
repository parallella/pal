#!/bin/bash

ESDK=${EPIPHANY_HOME}
ELIBS=${ESDK}/tools/host/lib:${LD_LIBRARY_PATH}
EHDF=${EPIPHANY_HDF}
ELDF=${ESDK}/bsps/current/internal.ldf

SCRIPT=$(readlink -f "$0")
EXEPATH=$(dirname "$SCRIPT")

e-reset

# Someone forgot this!
cd ./bin

sudo -E LD_LIBRARY_PATH=${ELIBS} EPIPHANY_HDF=${EHDF} $EXEPATH/bin/main.elf 0 0 4 4 0x12345678 

#> dotproduct.log

if [ $? -ne 0 ] 
then
    echo "$SCRIPT FAILED"
else
    echo "$SCRIPT PASSED"
fi
