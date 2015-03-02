#!/usr/bin/perl

#feel free to replace with bash script...

@List=`ls *.dat`;
chomp(@List);
foreach $test (@List){
    $test=~ s/\.dat//;
    system("./test_main.elf -f $test -i $test.dat -o $test.res");
}




