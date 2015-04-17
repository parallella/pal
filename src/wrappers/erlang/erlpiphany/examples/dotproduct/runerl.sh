#!/bin/bash

set -e

cd bin

./runerl.escript
#sudo -E LD_LIBRARY_PATH=${ELIBS} EPIPHANY_HDF=${EHDF} erl -pz ../../../ebin -pz ../../../priv

