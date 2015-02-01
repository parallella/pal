PAL: The Parallel Architectures Library
========================================

The Parallel Architectures Library (PAL) is a compact C library with optimized routines for vector math, synchronization, and multi-processor communication.

## Content
1.  [Why?](#why)
2.  [Design goals](#design-goals)  
3.  [License](#license)  
4.  [Contribution Wanted!](#contribution)  
5.  [A Simple Example](#a-simple-example)
6.  [Library API reference](#library-api-reference)  
6.0 [Syntax](#syntax)  
6.1 [Program Flow](#program-flow)  
6.2 [Data Movement](#data-movement)  
6.3 [Synchronization](#synchronization)  
6.3 [Basic Math](#math)  
6.5 [Basic DSP](#dsp)  
6.4 [Image Processing](#image-processing)  
6.6 [FFT (FFTW)](#fft)  
6.7 [Linar Algebra (BLAS)](#blas)  
6.8 [System Calls](#system-calls)  

----------------------------------------------------------------------
##Why?
As hard as we tried we could not find libraries that were a perfect fit for our design criteria. There a a number of projects and commercial products that offer most of the functionality of PAL but all of the existing offerings were either far too bulky or had the wrong license. In essence, the goal of the PAL effort is to provide an update the standard C libraries to address the trend towards massive multi-processor parallelism and SIMD computing. 

##Design Goals

* **Fast** (Super fast..but not always safe)
* **Compact** (as small as possible to fit with processors that have less than <<32KB of RAM)
* **Scalable** (thread and data scalable, limited only by the amount of local memory)
* **Portable across platforms** (deployable across different ISAs and system architectures)
* **Permissive license** (Apache 2.0 license to maximize overall use)

##License
The PAL source code is licensed under the Apache License, Version 2.0. 
See LICENSE for full license text unless otherwise specified.

##Contribution
Our goal is to make PAL a broad community project from day one. Some of these functions are tricky, but the biggest challenge with the PAL library is really the volume of simple functions.  The good news, if just 100 people contribute one function, we'll be done in a couple of days! If you know C, your are ready to contribute!!

Instructions for contributing can be found [HERE](CONTRIBUTING.md). 


##A Simple Example

**Manager Code**  

``` c
#include "pal_core.h"
#include <stdio.h>
#define N 16 
int main (int argc, char *argv[]){    

    //Stack variables
    char *file="./hello_task.elf";
    char *func="main";
    int status, i, all, nargs=1;   
    void* args[nargs];
    char argbuf[20];

    //Integer index into opaque structures
    int dev0, prog0, team0, mem[4];    

    //Execution setup
    dev0  = p_init(DEMO, 0);             //initialize device and team  
    prog0 = p_load(dev0, file, func, 0); //load a program from file system 
    all   = p_query(dev0, NODES);        //find number of nodes in system
    team0 = p_open(dev0, 0, all);        //create a team       

    //Running program
    for(i=0;i<all;i++){
	sprintf(argbuf, "%d", i); //string args needed to run main asis
	args[0]=&argbuf;
	status = p_run(prog0, team0, i, 1, nargs, args, 0);
    }
    p_barrier(team0);    //not needed
    p_close(team0);      //close team   
    p_finalize(dev0);    //finalize memory
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

##PROGRAM FLOW  
These program flow functions are used to manage the systems and to run programs.
All opaque objects are references through simple integers. 

FUNCTION     | NOTES
------------ | -------------
[p_init()](core/p_init.c)            | initialize the run time
[p_query()](core/p_query.c)          | query a device object
[p_load()](core/p_load.c)            | load binary elf file into memory
[p_run()](core/p_run.c)              | run a program on a team of processor
[p_open()](core/p_open.c)            | open a team of processors
[p_append()](core/p_append.c)        | add members to team
[p_remove()](core/p_remove.c)        | remove members from team
[p_close()](core/p_close.c)          | close a team of processors
[p_barrier()](core/p_barrier.c)      | wait for team to catch up
[p_fence()](core/p_fence.c)          | memory fence
[p_finalize()](core/p_finalize.c)    | cleans up run time

##MEMORY ALLOCATION  
These functions are used for creating memory objects. The function returns a unique integer for each new memory object. This integer can then be used by functions like p_read() and p_write() to access the data within the memory object.  

FUNCTION     | NOTES
------------ | -------------
[p_malloc()](core/p_malloc.c)        | allocate memory on local processor
[p_rmalloc()](core/p_rmalloc.c)      | allocate memory on remote processor
[p_free()](core/p_free.c)            | free memory

##DATA MOVEMENT  
The data movement functions move blocks of data between opaque memory objects and locations specified by pointers. The memory object is specified by a simple integer. The exception is the p_memcpy function which copies blocks of bytes within a shared memory architecture only.

FUNCTION     | NOTES
------------ | -------------
[p_broadcast()](core/p_broadcast.c) | broadcast operation
[p_gather()](core/p_gather.c)       | gather operation
[p_memcpy()](core/p_memcpy.c)       | fast memcpy()
[p_read()](core/p_read.c)           | read from a memory object
[p_scatter()](core/p_scatter.c)     | scatter operation
[p_write()](core/p_write.c)         | write to a memory object


##SYNCHRONIZATION  
The synchronization functions are useful for program sequencing and resource locking in shared memory systems.


FUNCTION     | NOTES
------------ | -------------
[p_mutex_lock()](core/p_mutex_lock.c)           | lock a mutex
[p_mutex_trylock()](core/p_mutex_trylock.c)     | try locking a mutex once
[p_mutex_unlock()](core/p_mutex_unlock.c)       | unlock (clear) a mutex
[p_mutex_init()](core/p_mutex_init.c)           | initialize a mutex
[p_atomic_add()](core/p_atomic_add.c)           | atomic fetch and add
[p_atomic_sub()](core/p_atomic_sub.c)           | atomic fetch and sub
[p_atomic_and()](core/p_atomic_and.c)           | atomic fetch and 'and'
[p_atomic_xor()](core/p_atomic_xor.c)           | atomic fetch and 'xor'
[p_atomic_or()](core/p_atomic_or.c)             | atomic fetch and 'or'
[p_atomic_swap()](core/p_atomic_swap.c)         | atomic exchange
[p_atomic_compswap()](core/p_atomic_compswap.c) | atomic compare and exchange

##MATH  
The math funtions are single threaded vectorized functions intended to run on a single processor. Math functions use pointers for input/output arguments and take in a separate variable to indicate the size of the vectors. Speed and size is a priority and some liberties have been taken with respect to accuracy and safety. 

FUNCTION     | NOTES
------------ | -------------
[p_abs()](math/p_abs.c)           | absolute value
[p_absdiff()](math/p_absdiff.c)   | absolute difference
[p_add()](math/p_add.c)           | add
[p_acos()](math/p_acos.c)         | arc cosine
[p_acosh()](math/p_acosh.c)       | arc hyperbolic cosine
[p_asin()](math/p_asin.c)         | arc sine
[p_asinh()](math/p_asinh.c)       | arc hyperbolic sine
[p_cbrt()](math/p_cbrt.c)         | cubic root
[p_cos()](math/p_cos.c)           | cosine
[p_cosh()](math/p_cosh.c)         | hyperbolic cosine 
[p_div()](math/p_div.c)           | division
[p_dot()](math/p_dot.c)           | dot product
[p_exp()](math/p_div.c)           | expontial
[p_ftoi()](math/p_ftoi.c)         | float to integer conversion
[p_itof()](math/p_itof.c)         | integer to float conversion
[p_inv()](math/p_inv.c)           | inverse
[p_invcbrt()](math/p_invcbrt.c)   | inverse cube root
[p_invsqrt()](math/p_invsqrt.c)   | inverse square root
[p_ln()](math/p_ln.c)             | natural log
[p_log10()](math/p_log10.c)       | denary log
[p_max()](math/p_max.c)           | finds max val
[p_min()](math/p_min.c)           | finds min val
[p_mean()](math/p_mean.c)         | mean operation
[p_median()](math/p_mean.c)       | finds middle value
[p_mode()](math/p_mode.c)         | finds most common value
[p_mul()](math/p_mul.c)           | multiplication
[p_popcount()](math/p_popcount.c) | count the number of bits set
[p_pow()](math/p_pow.c)           | element raised to a power
[p_rand()](math/p_rand.c)         | random number generator
[p_randinit()](math/p_rand.c)     | initialize random number generator
[p_sort()](math/p_sort.c)         | heap sort
[p_sin()](math/p_sin.c)           | sine
[p_sinh()](math/p_sinh.c)         | hyperbolic sine
[p_sqrt()](math/p_sqrt.c)         | square root
[p_sub()](math/p_sub.c)           | subtract
[p_sum()](math/p_sum.c)           | sum of all vector elements
[p_sumsq()](math/p_sumsq.c)       | sum of all vector squared elements 
[p_tan()](math/p_tan.c)           | tangent
[p_tanh()](math/p_tanh.c)         | hyperbolic tangent

##DSP  
The digital signal processing (dsp) funtions are similar to the math functions
in that they are single threaded vectorized functions intended to run on a 
single core. Also, just like the math functions they take in pointers for 
input/output arguments and a separate variable to indicate the size of the 
vectors. Speed and size is a priority and some liberties have been taken with 
respect to accuracy and safety.

FUNCTION     | NOTES
------------ | -------------
[p_acorr()](dsp/p_acorr.c)   | autocorrelation (r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1))
[p_conv()](dsp/p_conv.c)     | convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1)
[p_xcorr()](dsp/p_xcorr.c)   | correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1)
[p_fir()](dsp/p_fir.c)       | FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1)
[p_firdec()](dsp/p_firdec.c) | FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firint()](dsp/p_firint.c) | FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firsym()](dsp/p_firsym.c) | FIR symmetric form
[p_iir()](dsp/p_iir.c)       | IIR filter

##IMAGE PROCESSING  
The image processing functions work on 2D arrays of data and use the same
argument passing conventions as the dsp and math functions. 

FUNCTION     | NOTES
------------ | -------------
[p_box3x3()](image/p_box3x3.c)         | box filter (3x3)
[p_conv2d()](image/p_conv2d.c)         | 2d convolution
[p_gauss3x3()](image/p_gauss3x3.c)     | gaussian blur filter (3x3)
[p_median3x3()](image/p_median3x3.c)   | median filter (3x3)
[p_laplace3x3()](image/p_laplace3x3.c) | laplace filter (3x3)
[p_prewitt3x3()](image/p_prewitt3x3.c) | prewitt filter (3x3)
[p_sad8x8()](image/p_sad8x8.c)         | sum of absolute differences (8x8)
[p_sad16x16()](image/p_sad16x16.c)     | sum of absolute differences (16x16)
[p_sobel3x3()](image/p_sobel3x3.c)     | sobel filter (3x3)
[p_scharr3x3()](image/p_scharr3x3.c)   | scharr filter (3x3)

##FFT  

* An FFTW like interface

##BLAS  

* A port of the BLIS library

##SYSTEM CALLS

* Bionic libc implementation as starting point..

