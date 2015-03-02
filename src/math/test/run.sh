#!/bin/bash

for file in $(find . -iname '*.dat'); do 
    ${file//[xyz]/_}
    echo "./test_main.elf -f ${file%.*} -i ${file} -o ${file%.*}.res"
done
