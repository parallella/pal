PAL: The Parallel Architectures Library
========================================

The Parallel Architectures Library (PAL) is a free and open-source C-libary that provides optimized math routines and low-level synchronization primitives for parallel programming. 

##Design Goals:

* **Fast** (No tricks held back)
* **Compact** (Designed for tiny CPUs with less than 32KB of local memory)
* **Scalable** (Native vector parallelism)
* **Open** (Permissive Apache 2.0 Licensing)

A complete list of linked functions can be found at the end of this file 

##Licensing
PAL is licensed under the Apache License, Version 2.0. See LICENSE for full license text.

##Contributing
PAL is an ambitious project and neeeds all the help it can get. If you have anything to contribute, please do!! Instructions for get started can be found [HERE](CONTRIBUTING.md). 

##Coding style 
* C99
* "K&R coding style, 4 spaces for tabs"
* 

##API Style
The PAL library provides a set of "native" functions (fastest) and a set of "vector object" functions (easiest). The native funtions uses ONLY native C data types and explicit array indexing, while the array based functions use opaque array objects.

##Naming convention
* All functions start with "p_"
* Function data types are indicated as follows:
  8u   : 8 bit unsigned
  8s   : 8 bit signed
  16u  : 16 bit integer unsigned
  16s  : 16 bit integer signed
  16q  : 0.15 signed fractional
  16qc : 0.15 signed complex fractional 
  32u  : 32 bit unsigned integer
  32s  : 32 bit signed integer
  32f  : 32 bit IEEE float point
  32fc : 32 bit IEEE complex float         
  64f  : 64 bit IEEE floating point


##API Native Example Style
``` c
/*Function: Vector addition
            y[n-1:0]=a[n-1:0]+b[n-1:0]

  Arguments: 
  
  n is the number of elements in the vector
  a is a pointer to an intput array of floats
  b is a pointer to an input array of floats
  y is a pointer to an output array of floats
 */
 
void p_add_32f( int n, float* a, float* b, float* y );

```

PAL Library Functions
========================================
## HAL

**Memory Management (shared memory):**
* p_read()
* p_write()
* p_memcpy()
* p_memcpy_scatter()
* p_memcpy_gather()
* p_memcpy_bcast()
* p_memalloc()
* p_memfree()
* p_memptr()


**Program Execution (shared memory):**
* p_init()
* p_open()
* p_close()
* p_finalize()
* p_load()
* p_exec()


## IPC

**Mutex:**
* p_mutex_attr_init()
* p_mutex_atr_destroy()
* p_mutex_attr_setdevice()
* p_mutex_init()
* p_mutex_destroy()
* p_mutex_lock()
* p_mutex_unlock()
* p_mutex_trylock()

**Atomics:**
* p_atomic_init()
* p_atomic_exchange()
* p_atomic_compare_exchange()
* p_atomic_fetch_add()
* p_atomic_fetch_sub()
* p_atomic_fetch_or()
* p_atomic_fetch_xor()
* p_atomic_fetch_and()

**Memory ordering:**
* p_mem_flush()

**Barrier:**
* p_barrier_init()
* p_barrier_wait()

## MATH
FUNCTION | NOTES
------------ | -------------
[p_abs()](math/p_abs.c)   | absolute value
[p_absdiff()](math/p_absdiff.c)   | absolute difference
[p_add()](math/p_add.c)   | add
[p_acos()](math/p_acos.c) | arc cosine
[p_acosh()](math/p_acosh.c) | arc hyperbolic cosine
[p_asin()](math/p_asin.c) | arc sine
[p_asinhh()](math/p_asinh.c) | arc hyperbolic sine
[p_cbrt()](math/p_cbrt.c) | cubic root
[p_cos()](math/p_cos.c) | cosine
[p_cosh()](math/p_cosh.c) | hyperbolic cosine
[p_div()](math/p_div.c) | division
[p_exp()](math/p_div.c) | expontial
[p_inv()](math/p_inv.c) | inverse
[p_invcbrt()](math/p_invcbrt.c) | inverse cube root
[p_invsqrt()](math/p_invcbrt.c) | inverse square root
[p_ln()](math/p_invcbrt.c) | natural log
[p_log10()](math/p_log10.c) | denary log
[p_maxval()](math/p_maxval.c) | finds max val
[p_minval()](math/p_minval.c) | finds min val
[p_mean()](math/p_mean.c) | mean operation
[p_median()](math/p_mean.c) | finds middle value
[p_mode()](math/p_mode.c) | finds most common value
[p_mul()](math/p_mul.c) | multiplication
[p_pow()](math/p_pow.c) | element raised to a power
[p_sin()](math/p_sin.c) | sine
[p_sincos()](math/p_sincos.c) | sine and cos results
[p_sinh()](math/p_sinh.c) | hyperbolic sine
[p_sqrt()](math/p_sqrt.c) | square root
[p_sub()](math/p_sub.c) | subtract
[p_sum()](math/p_sum.c) | sum of all vector elements
[p_sumsq()](math/p_sumsq.c) | sum of all vector squared elements 
[p_tan()](math/p_tan.c) | tangent
[p_tanh()](math/p_tanh.c) | hyperbolic tangent
[p_dot()](math/p_dot.c) | dot product
















[p_itof()](math/p_itof.c) | integer to float conversion
[p_ftoi()](math/p_ftoi.c) | float to integer conversion

 
## DSP

## FFT



