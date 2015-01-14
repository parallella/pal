PAL: The Parallel Architectures Library
========================================

The Parallel Architectures Library (PAL) is a collection of C libraries that
faciliate high performance computation, and data movement, and synchronization.

# Content
1.  [Design goals](#design-goals)  
2.  [License](#license)  
3.  [Pay it forward](#pay-it-forward)  
4.  [Simple PAL Example](#simple-pal-example)
5.  [Library API reference](#pal-api-reference)  
5.0 [Syntax](#syntax)  
5.1 [Program Excution](#program-execution)  
5.2 [Data Movement](#data-movement)  
5.3 [Synhcronization](#synchronization)  
5.3 [Basic Math](#basic-math)  
5.5 [Basic DSP](#basic-dsp)  
5.4 [Image Processing](#image-processing)  
5.6 [FFT](#fft)  
5.7 [Linar Algebra (BLAS)](#blas)  
-vector operations
-matrix-vector operations
-matrix-matrix operations
5.8 [System Calls](#system-calls)

----------------------------------------------------------------------

##Design Goals

* **Fast**     (All about speed. No belt...no suspenders)
* **Open**     (Permissive Apache 2.0/BSD/MIT licensing)
* **Compact**  (Developed for CPUs with limited local memory)
* **Scalable** (Support thread and data scaling)
* **Portable** (Across architectures and run time environments)   

##License
The PAL source code is licensed under the Apache License, Version 2.0. See LICENSE for full license text unless otherwise specified.

##Pay it forward
Seriously, pay it forward! Instructions for contributing can be found [HERE](CONTRIBUTING.md). 

##An Example

**Boss Code
``` c
#include <stdio.h>
#include <stdlib.h>
#include <pal_core.h>
#define N 32

int main(int argc, char *argv[]){
    
    //Some variable declarations
    size_t bufsize = N * sizeof(float);   

    //Initialize system
    pal_dev_t dev0 =  pal_init(EPIPHANY, 0);

    //Query system for information
    int *all   = pal_query(dev0, ALL);    //list of all all processor ids
    int *myid  = pal_query(dev0, WHOAMI); //this processor
        
    //Load an ELF file from the file system
    pal_program_t prog0 = pal_load(dev0, "./my.elf");

    //Create a working team
    pal_team_t team0 = pal_open(dev0, all, 0);
    
    //Run the program on a team of processors
    pal_run(team0, prog0, argc, argv, 0);

    //Close down the team
    pal_close(team0);

    //Close down device connection
    pal_finalize(dev0);    
}

```

**Worker Code
``` c

```


PAL LIBRARY API REFERENCE
========================================
## 

##PROGRAM EXECUTION

FUNCTION     | NOTES
------------ | -------------
[p_init()](hal/p_init.c)                 | initialize the run time
[p_load()](hal/p_load.c)                 | load binary elf file into memory
[p_open()](hal/p_open.c)                 | open a team of processors
[p_run()](hal/p_run.c)                   | run a program on a team of processor
[p_close()](hal/p_close.c)               | close a team of processors
[p_finalize()](hal/p_finalize.c)         | cleans up run time
[p_malloc()](hal/p_malloc.c)             | dynamic memory allocator
[p_free()](hal/p_free.c)                 | free up dynamic memory
[p_barrier()](hal/p_barrier.c)           | team barrier wait

##DATA MOVEMENT

FUNCTION     | NOTES
------------ | -------------
[p_write()](memory/p_write.c)        | write to global memory
[p_read()](memory/p_read.c)          | read from global memory
[p_copy()](memory/p_copy.c)          | copy memory
[p_scatter()](memory/p_scatter.c)    | copy scatter
[p_gather()](memory/p_gather.c)      | copy gather
[p_bcast()](memory/p_bcast.c)        | copy broadcast
[p_malloc()](memory/p_malloc.c)      | dynamic memory allocator
[p_free()](memory/p_free.c)          | free up dynamic memory
[p_flush()](memory/p_flush.c)        | flush a physical memory read/write path  

##SYNCHRONIZATION
FUNCTION     | NOTES
------------ | -------------
[p_mutex_lock()](hal/p_mutex_lock.c)                           | lock a mutex
[p_mutex_trylock()](hal/p_mutex_trylock.c)                     | try locking a mutex once
[p_mutex_unlock()](hal/p_mutex_unlock.c)                       | unlock (clear) a mutex
[p_mutex_init()](hal/p_mutex_init.c)                           | initialize a mutex
[p_atomic_add()](hal/p_atomic_add.c)                           | atomic fetch and add
[p_atomic_sub()](hal/p_atomic_sub.c)                           | atomic fetch and sub
[p_atomic_and()](hal/p_atomic_and.c)                           | atomic fetch and logical 'and'
[p_atomic_xor()](hal/p_atomic_xor.c)                           | atomic fetch and logical 'xor'
[p_atomic_or()](hal/p_atomic_or.c)                             | atomic fetch and logical 'or'
[p_atomic_nand()](hal/p_atomic_nand.c)                         | atomic fetch and logical 'nand'
[p_atomic_exchange()](hal/p_atomic_exchange .c)                | atomic exhchange (swap)
[p_atomic_compare_exchange()](hal/p_atomic_compare_exchange.c) | atomic compare and exchange

## BASIC MATH

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
[p_invsqrt()](math/p_invcbrt.c)   | inverse square root
[p_ln()](math/p_invcbrt.c)        | natural log
[p_log10()](math/p_log10.c)       | denary log
[p_max()](math/p_max.c)           | finds max val
[p_min()](math/p_min.c)           | finds min val
[p_mean()](math/p_mean.c)         | mean operation
[p_median()](math/p_mean.c)       | finds middle value
[p_mode()](math/p_mode.c)         | finds most common value
[p_mul()](math/p_mul.c)           | multiplication
[p_popcount()](math/p_popcount.c) | count the number of bits set
[p_pow()](math/p_pow.c)           | element raised to a power
[p_sort()](math/p_sort.c)         | heap sort
[p_sin()](math/p_sin.c)           | sine
[p_sincos()](math/p_sincos.c)     | sine and cos results
[p_sinh()](math/p_sinh.c)         | hyperbolic sine
[p_sqrt()](math/p_sqrt.c)         | square root
[p_sub()](math/p_sub.c)           | subtract
[p_sum()](math/p_sum.c)           | sum of all vector elements
[p_sumsq()](math/p_sumsq.c)       | sum of all vector squared elements 
[p_tan()](math/p_tan.c)           | tangent
[p_tanh()](math/p_tanh.c)         | hyperbolic tangent

##BASIC DSP

FUNCTION     | NOTES
------------ | -------------
[p_acorr()](dsp/p_acorr.c)   | autocorrelation (r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1))
[p_conv()](dsp/p_conv.c)     | convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1)
[p_corr()](dsp/p_corr.c)     | correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1)
[p_fir()](dsp/p_fir.c)       | FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1)
[p_firdec()](dsp/p_firdec.c) | FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firint()](dsp/p_firint.c) | FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firlat()](dsp/p_firlat.c) | FIR filter lattice form
[p_firsym()](dsp/p_firsym.c) | FIR symmetric form
[p_iir()](dsp/p_iir.c)       | IIR filter

##IMAGE PROCESSING

FUNCTION     | NOTES
------------ | -------------
[p_average3x3()](image/p_average3x3.c) | moving average filter (3x3)
[p_box3x3()](image/p_box3x3.c)         | box filter (3x3)
[p_conv2d()](image/p_conv2d.c)         | 2d convolution
[p_gauss3x3()](image/p_gauss3x3.c)     | gaussian blur filter (3x3)
[p_mad8x8()](image/p_mad8x8.c)         | sum of mean differences (8x8)
[p_mad16x16()](image/p_mad16x16.c)     | sum of mean differences (16x16)
[p_median3x3()](image/p_median3x3.c)   | median filter (3x3)
[p_laplace3x3()](image/p_laplace3x3.c) | laplace filter (3x3)
[p_prewitt3x3()](image/p_prewitt3x3.c) | prewitt filter (3x3)
[p_sad8x8()](image/p_sad8x8.c)         | sum of absolute differences (8x8)
[p_sad16x16()](image/p_sad16x16.c)     | sum of absolute differences (16x16)
[p_sobel3x3()](image/p_sobel3x3.c)     | sobel filter (3x3)
[p_scale2d()](image/p_scale2d.c)       | 2d image scaling
[p_scharr3x3()](image/p_scharr3x3.c)   | scharr filter (3x3)

##FFT  
* An FFTW like interface

##BLAS

*Will implement a port of the BLIS library

##SYSTEM CALLS

* Bionic libc implementation as starting point
