PAL: The Parallel Architectures Library
========================================

The Parallel Architectures Library (PAL) is a collection of C libraries that
faciliate high performance computation, synchronization, and data movement.

# Content
1. [Design goals](#design-goals)  
2. [License](#license)  
3. [Pay it forward](#pay-it-forward)  
4. [Library API reference](#pal-api-reference)  
4.0 [Syntax](#syntax)  
4.1 [Memory management (PAL)](#memory-management)  
4.2 [Program excution (PAL)](#program-execution)  
4.3 [Basic math (PAL)](#basic-math)  
4.5 [Basic DSP (PAL)](#dsp)  
4.4 [Image processing (PAL)](#image-processing)  
4.6 [FFT (PAL)](#fft)  
4.7 [Linar algebra (BLAS)](#blas)  
4.8 [System Calls (LIBC-BIONIC)](#system-calls)  
4.9 [Mutex (POSIX-BIONIC)](#mutex)  
4.A [Threads (POSIX-BIONIC)](#threads)  
4.B [Atomics (C11)](#atomics) 

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

PAL API REFERENCE
========================================
## 

##MEMORY MANAGEMENT

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
[p_flush()](memory/p_flush.c)       | flush a physical memory read/write path  

##PROGRAM EXECUTION

FUNCTION     | NOTES
------------ | -------------
[p_init()](hal/p_init.c)         | initialize the run time
[p_load()](hal/p_load.c)         | load binary elf file into an array
[p_open()](hal/p_open.c)         | open a set of slave processors
[p_exec()](hal/p_exec.c)         | run a program on a slave processor
[p_close()](hal/p_close.c)       | close a set of slave processors
[p_finalize()](hal/p_finalize.c) | close down run time


## MATH
FUNCTION     | NOTES
------------ | -------------
[p_abs()](math/p_abs.c)          | absolute value
[p_absdiff()](math/p_absdiff.c)  | absolute difference
[p_add()](math/p_add.c)          | add
[p_acos()](math/p_acos.c)        | arc cosine
[p_acosh()](math/p_acosh.c)      | arc hyperbolic cosine
[p_asin()](math/p_asin.c)        | arc sine
[p_asinh()](math/p_asinh.c)      | arc hyperbolic sine
[p_cbrt()](math/p_cbrt.c)        | cubic root
[p_cos()](math/p_cos.c)          | cosine
[p_cosh()](math/p_cosh.c)        | hyperbolic cosine
[p_div()](math/p_div.c)          | division
[p_dot()](math/p_dot.c)          | dot product
[p_exp()](math/p_div.c)          | expontial
[p_ftoi()](math/p_ftoi.c)        | float to integer conversion
[p_itof()](math/p_itof.c)        | integer to float conversion
[p_inv()](math/p_inv.c)          | inverse
[p_invcbrt()](math/p_invcbrt.c)  | inverse cube root
[p_invsqrt()](math/p_invcbrt.c)  | inverse square root
[p_ln()](math/p_invcbrt.c)       | natural log
[p_log10()](math/p_log10.c)      | denary log
[p_max()](math/p_max.c)          | finds max val
[p_min()](math/p_min.c)          | finds min val
[p_mean()](math/p_mean.c)        | mean operation
[p_median()](math/p_mean.c)      | finds middle value
[p_mode()](math/p_mode.c)        | finds most common value
[p_mul()](math/p_mul.c)          | multiplication
[p_pow()](math/p_pow.c)          | element raised to a power
[p_sin()](math/p_sin.c)          | sine
[p_sincos()](math/p_sincos.c)    | sine and cos results
[p_sinh()](math/p_sinh.c)        | hyperbolic sine
[p_sqrt()](math/p_sqrt.c)        | square root
[p_sub()](math/p_sub.c)          | subtract
[p_sum()](math/p_sum.c)          | sum of all vector elements
[p_sumsq()](math/p_sumsq.c)      | sum of all vector squared elements 
[p_tan()](math/p_tan.c)          | tangent
[p_tanh()](math/p_tanh.c)        | hyperbolic tangent

## DSP

FUNCTION     | NOTES
------------ | -------------
[p_acorr()](math/p_acorr.c) | autocorrelation (r[j] = sum ( x[j+k] * x[k] ), k=0..(n-j-1))
[p_conv()](math/p_conv.c) | convolution: r[j] = sum ( h[k] * x[j-k), k=0..(nh-1)
[p_corr()](math/p_corr.c) | correlation: r[j] = sum ( x[j+k] * y[k]), k=0..(nx+ny-1)
[p_fir()](math/p_fir.c) | FIR filter direct form: r[j] = sum ( h[k] * x [j-k]), k=0..(nh-1)
[p_firdec()](math/p_firdec.c) | FIR filter with decimation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firint()](math/p_firint.c) | FIR filter with inerpolation: r[j] = sum ( h[k] * x [j*D-k]), k=0..(nh-1)
[p_firlat()](math/p_firlat.c) | FIR filter lattice form
[p_firsym()](math/p_firsym.c) | FIR symmetric form
[p_iir()](math/p_iir.c) | IIR filter

## IMAGE PROCESSING

FUNCTION     | NOTES
------------ | -------------
[p_average3x3()](math/p_average3x3.c) | moving average filter (3x3)
[p_box3x3()](math/p_box3x3.c)         | box filter (3x3)
[p_conv2d()](math/p_conv2d.c)         | 2d convolution
[p_gauss3x3()](math/p_gauss3x3.c)     | gaussian blur filter (3x3)
[p_mad8x8()](math/p_mad8x8.c)         | sum of mean differences (8x8)
[p_mad16x16()](math/p_mad16x16.c)     | sum of mean differences (16x16)
[p_median3x3()](math/p_median3x3.c)   | median filter (3x3)
[p_laplace3x3()](math/p_laplace3x3.c) | laplace filter (3x3)
[p_prewitt3x3()](math/p_prewitt3x3.c) | prewitt filter (3x3)
[p_sad8x8()](math/p_sad8x8.c)         | sum of absolute differences (8x8)
[p_sad16x16()](math/p_sad16x16.c)     | sum of absolute differences (16x16)
[p_sobel3x3()](math/p_sobel3x3.c)     | sobel filter (3x3)
[p_scale2d()](math/p_scale2d.c)       | 2d image scaling
[p_scharr3x3()](math/p_scharr3x3.c)   | scharr filter (3x3)




## FFT

FUNCTION     | NOTES
------------ | -------------

## BLAS

FUNCTION     | NOTES
------------ | -------------

## SYSTEM CALLS

FUNCTION     | NOTES
------------ | -------------

## MUTEX

FUNCTION     | NOTES
------------ | -------------

## THREADS

FUNCTION     | NOTES
------------ | -------------

## ATOMICS

FUNCTION     | NOTES
------------ | -------------
