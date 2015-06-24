PAL: The Parallel Architectures Library
========================================

[![Build Status](https://travis-ci.org/parallella/pal.svg?branch=master)](https://travis-ci.org/parallella/pal)

The Parallel Architectures Library (PAL) is a compact C library with optimized routines for math, synchronization, and inter-processor communication.

## Content
1.  [Why?](#why)

2.  [Design goals](#design-goals)  

3.  [License](#license)  

4.  [Contribution Wanted!](#contribution)  

5.  [A Simple Example](#a-simple-example)

6.  [Build Instructions](#build-instructions)

7.  [Library API reference](#library-api-reference)  
7.0 [Syntax](#syntax)  
7.1 [Program Flow](#program-flow)  
7.2 [Data Movement](#data-movement)  
7.3 [Synchronization](#synchronization)  
7.3 [Basic Math](#math)  
7.5 [Basic DSP](#dsp)  
7.4 [Image Processing](#image-processing)  
7.6 [FFT (FFTW)](#fft)  
7.7 [Linar Algebra (BLAS)](#blas)  
7.8 [System Calls](#system-calls)  

8   [Status Report](#status-report)  

9   [Benchmarking](#benchmarking)  

----------------------------------------------------------------------
##Why?
Any sane and informed person knows that the future of computing is massively parallel. Unfortunately the energy needed to escape the current "von Neumann potential well" seems to be approaching infinity. The legacy programming stack is so effective and so easy to use that developers and companies simply cannot afford to choose the better (parallel) solution. To make parallel computing ubiquitous our only choice is to rewrite the whole software stack from scratch, including: algorithms, run-times, libraries, and applications. The goal of the Parallel Architectures Library project is to establish the lowest layer of this brave new programming stack.

##Design Goals

* **Fast** (Super fast but no "belt AND suspenders")
* **Compact** (Small enough to work for memory limited processors with <32KB RAM)
* **Scalable** (Thread and data scalable)
* **Portable** (Portable across different ISAs and systems)
* **Permissive** (Apache 2.0 license to maximize industry adoption)

##License
The PAL source code is licensed under the Apache License, Version 2.0. 
See LICENSE for full license text unless otherwise specified.

##Contribution
Our goal is to make PAL a broad community project from day one. If just 100 people contribute one function each, we'll be done in a couple of days! If you know C, you are ready to contribute!!

Instructions for contributing can be found [HERE](CONTRIBUTING.md). 

##Build Instructions

###Install Prerequisites
``` bash
$ sudo apt-get install libtool build-essential pkg-config autoconf doxygen check
```

###Build Sequence

``` bash
$ ./bootstrap
$ ./configure
$ make
```

###Testing

To run the automated unit tests you need to run
``` bash
$ make check
```

For this to succeed you need to have the check unit test library (http://check.sourceforge.net).
See [prerequisites](#install-prerequisites).


##A Simple Example
The following sample shows how to use PAL launch a simple task on a remote processor within the system. The program flow should be familiar to anyone who has used accelerator programming frameworks. 

**Manager Code**  

``` c
#include <pal.h>
#include <stdio.h>
#define N 16
int main(int argc, char *argv[])
{

    // Stack variables
    char *file = "./hello_task.elf";
    char *func = "main";
    int status, i, all, nargs = 1;
    char *args[nargs];
    char argbuf[20];

    // References as opaque structures
    p_dev_t dev0;
    p_prog_t prog0;
    p_team_t team0;
    p_mem_t mem[4];

    // Execution setup
    dev0 = p_init(P_DEV_DEMO, 0);        // initialize device and team
    prog0 = p_load(dev0, file, func, 0); // load a program from file system
    all = p_query(dev0, P_PROP_NODES);   // find number of nodes in system
    team0 = p_open(dev0, 0, all);        // create a team

    // Running program
    for (i = 0; i < all; i++) {
        sprintf(argbuf, "%d", i); // string args needed to run main asis
        args[0] = argbuf;
        status = p_run(prog0, team0, i, 1, nargs, args, 0);
    }
    p_wait(team0);    // not needed
    p_close(team0);   // close team
    p_finalize(dev0); // finalize memory

    return 0;
}
```

**Worker Code (hello_task.elf)**  
``` c
#include <stdio.h>
int main(int argc, char* argv[]){
    int pid=0;
    int i;
    pid=atoi(argv[2]);
    printf("--Processor %d says hello!--\n", pid);
    return i;
}
```

PAL LIBRARY API REFERENCE
========================================
##SYNTAX 

##PROGRAM FLOW  
These program flow functions are used to manage the system and to execute programs. All PAL objects are referenced via handles (opaque objects).

FUNCTION     | NOTES         
------------ | ------------- 
[p_init()](src/base/p_init.c)            | initialize the run time
[p_query()](src/base/p_query.c)          | query a device object
[p_load()](src/base/p_load.c)            | load binary elf file into memory
[p_run()](src/base/p_run.c)              | run a program on a team of processor
[p_open()](src/base/p_open.c)            | open a team of processors
[p_append()](src/base/p_append.c)        | add members to team
[p_remove()](src/base/p_remove.c)        | remove members from team
[p_close()](src/base/p_close.c)          | close a team of processors
[p_barrier()](src/base/p_barrier.c)      | team barrier
[p_wait()](src/base/p_wait.c)            | wait for team to finish
[p_fence()](src/base/p_fence.c)          | memory fence
[p_finalize()](src/base/p_finalize.c)    | cleans up run time
[p_get_err()](src/base/p_get_err.c)      | get error code (if any).

##MEMORY ALLOCATION  
These functions are used for creating memory objects.
The functions return a unique PAL handle for each new memory object. This handle can then be used by functions like p_read() and p_write() to access data within the memory object.  

FUNCTION     | NOTES        | STATUS
------------ | -------------|-----------
[p_malloc()](src/base/p_malloc.c)        | allocate memory on local processor
[p_rmalloc()](src/base/p_rmalloc.c)      | allocate memory on remote processor
[p_free()](src/base/p_free.c)            | free memory

##DATA MOVEMENT  
The data movement functions move blocks of data between opaque memory objects and locations specified by pointers. The memory object is specified by a PAL handle returned by a previous API call. The exception is the p_memcpy function which copies blocks of bytes within a shared memory architecture only.

FUNCTION     | NOTES
------------ | -------------
[p_gather()](src/base/p_gather.c)       | gather operation
[p_memcpy()](src/base/p_memcpy.c)       | fast memcpy()
[p_read()](src/base/p_read.c)           | read from a memory object
[p_scatter()](src/base/p_scatter.c)     | scatter operation
[p_write()](src/base/p_write.c)         | write to a memory object


##SYNCHRONIZATION  
The synchronization functions are useful for program sequencing and resource locking in shared memory systems.


FUNCTION     | NOTES
------------ | -------------
[p_mutex_lock()](src/base/p_mutex_lock.c)           | lock a mutex
[p_mutex_trylock()](src/base/p_mutex_trylock.c)     | try locking a mutex once
[p_mutex_unlock()](src/base/p_mutex_unlock.c)       | unlock (clear) a mutex
[p_mutex_init()](src/base/p_mutex_init.c)           | initialize a mutex
[p_atomic_add()](src/base/p_atomic_add.c)           | atomic fetch and add
[p_atomic_sub()](src/base/p_atomic_sub.c)           | atomic fetch and sub
[p_atomic_and()](src/base/p_atomic_and.c)           | atomic fetch and 'and'
[p_atomic_xor()](src/base/p_atomic_xor.c)           | atomic fetch and 'xor'
[p_atomic_or()](src/base/p_atomic_or.c)             | atomic fetch and 'or'
[p_atomic_swap()](src/base/p_atomic_swap.c)         | atomic exchange
[p_atomic_compswap()](src/base/p_atomic_compswap.c) | atomic compare and exchange

##MATH  
The math functions replace the traditional math lib functions and extend them to include support for data as well as task parallelism. 

FUNCTION     | NOTES         
------------ | ------------- 
[p_abs()](src/math/p_abs.c)           | absolute value  
[p_absdiff()](src/math/p_absdiff.c)   | absolute difference  
[p_add()](src/math/p_add.c)           | add  
[p_acos()](src/math/p_acos.c)         | arc cosine  
[p_acosh()](src/math/p_acosh.c)       | arc hyperbolic cosine  
[p_asin()](src/math/p_asin.c)         | arc sine  
[p_asinh()](src/math/p_asinh.c)       | arc hyperbolic sine  
[p_cbrt()](src/math/p_cbrt.c)         | cubic root                    
[p_cos()](src/math/p_cos.c)           | cosine  
[p_cosh()](src/math/p_cosh.c)         | hyperbolic cosine  
[p_div()](src/math/p_div.c)           | division  
[p_dot()](src/math/p_dot.c)           | dot product  
[p_exp()](src/math/p_exp.c)           | exponential
[p_ftoi()](src/math/p_ftoi.c)         | float to  
[p_itof()](src/math/p_itof.c)         | integer to float conversion  
[p_inv()](src/math/p_inv.c)           | inverse  
[p_invcbrt()](src/math/p_invcbrt.c)   | inverse cube root  
[p_invsqrt()](src/math/p_invsqrt.c)   | inverse square root  
[p_ln()](src/math/p_ln.c)             | natural log  
[p_log10()](src/math/p_log10.c)       | denary log  
[p_max()](src/math/p_max.c)           | finds max val  
[p_min()](src/math/p_min.c)           | finds min val  
[p_mean()](src/math/p_mean.c)         | mean operation  
[p_median()](src/math/p_median.c)     | finds middle value  
[p_mode()](src/math/p_mode.c)         | finds most common value  
[p_mul()](src/math/p_mul.c)           | multiplication  
[p_popcount()](src/math/p_popcount.c) | count the number of bits set  
[p_pow()](src/math/p_pow.c)           | element raised to a power  
[p_rand()](src/math/p_rand.c)         | random number generator  
[p_randinit()](src/math/p_rand.c)     | init random number generator  
[p_sort()](src/math/p_sort.c)         | heap sort  
[p_sin()](src/math/p_sin.c)           | sine  
[p_sinh()](src/math/p_sinh.c)         | hyperbolic sine  
[p_sqrt()](src/math/p_sqrt.c)         | square root  
[p_stddev()](src/math/p_stddev.c)     | calculates standard deviation  
[p_sub()](src/math/p_sub.c)           | subtract  
[p_sum()](src/math/p_sum.c)           | sum of all vector elements  
[p_sumsq()](src/math/p_sumsq.c)       | sum of all squared elements  
[p_tan()](src/math/p_tan.c)           | tangent  
[p_tanh()](src/math/p_tanh.c)         | hyperbolic tangent  

##DSP  
The digital signal processing (DSP) functions follow the same convention as the math function set.

FUNCTION     | NOTES
------------ | -------------
[p_acorr()](src/dsp/p_acorr.c)   | autocorrelation (r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1))
[p_conv()](src/dsp/p_conv.c)     | convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1)
[p_xcorr()](src/dsp/p_xcorr.c)   | correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1)
[p_fir()](src/dsp/p_fir.c)       | FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1)
[p_firdec()](src/dsp/p_firdec.c) | FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firint()](src/dsp/p_firint.c) | FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firsym()](src/dsp/p_firsym.c) | FIR symmetric form
[p_iir()](src/dsp/p_iir.c)       | IIR filter

##IMAGE PROCESSING  
The image processing functions follow the same convention as the math function set.

FUNCTION     | NOTES
------------ | -------------
[p_box3x3()](src/image/p_box3x3.c)         | box filter (3x3)
[p_conv2d()](src/image/p_conv2d.c)         | 2d convolution
[p_gauss3x3()](src/image/p_gauss3x3.c)     | gaussian blur filter (3x3)
[p_median3x3()](src/image/p_median3x3.c)   | median filter (3x3)
[p_laplace3x3()](src/image/p_laplace3x3.c) | laplace filter (3x3)
[p_prewitt3x3()](src/image/p_prewitt3x3.c) | prewitt filter (3x3)
[p_sad8x8()](src/image/p_sad8x8.c)         | sum of absolute differences (8x8)
[p_sad16x16()](src/image/p_sad16x16.c)     | sum of absolute differences (16x16)
[p_sobel3x3()](src/image/p_sobel3x3.c)     | sobel filter (3x3)
[p_scharr3x3()](src/image/p_scharr3x3.c)   | scharr filter (3x3)

##FFT  

* An FFTW like interface

##BLAS  

* A port of the BLIS library?

##SYSTEM CALLS

* Bionic libc implementation as starting point..

STATUS REPORT
========================================
https://parallella.github.io/pal-stats/summary.html

BENCHMARKING
========================================
LINK
=======
* TBD

BENCHMARKING
========================================
E=Epiphany  
X=x86  
A=ARM  
CC=Clock cycles  
  
FUNCTION  | E-CC     | E-SIZE   | A-CC     | A-SIZE   | X-CC     |X-SIZE    |   
----------|----------|----------|----------|----------|----------|----------|  
p_add()   | TBD      | TBD      | TBD      | TBD      | TBD      | TBD      |  
...       | ...      | ...      | ...      | ...      | ...      | ...      |  

