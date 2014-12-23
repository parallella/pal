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

##API Style Example
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

**Conversion:**
``` c
/*integer to float conversion on a vector*/
void p_itof(int n, int* a, float* y);

/*float to integer conversion*/
void p_ftoi(int n, float* a, int* y );
```

**Standard Math Functions:**
``` c

/*inverse cosine*/
void p_acos_32f(int n, float* a, float* y );

/*inverse hyperbolic cosine*/
void p_acosh_32f(int n, float* a, float* y );

/*inverse sine*/
void p_asin_32f(int n, float* a, float* y );

/*inverse tanget*/
void p_atan_32f(int n, float* a, float* y );

/*four quadrant inverse tangent*/
void p_atan2_32f(int n, float* a, float* b, float* y );

/*cube root*/
void p_cbrt_32f(int n, float* a, float* y );

/*cosine*/
void p_cos_32f(int n, float* a, float* y );

/*hyperpolic cosine*/
void p_cosh_32f(int n, float* a, float* y );

/*exponential*/
void p_exp_32f(int n, float* a, float* y );

/*inverse cube root*/
void p_icbrt_32f(int n, float* a, float* y );

/*natural log*/
void p_ln_32f(int n, float* a, float* y );

/*denary logarithm*/
void p_log10_32f(int n, float* a, float* y );

/*element raised to a specific power*/
void p_pow_32f(int n, float* a, float* b, float* y );

/*sine*/
void p_sin_32f(int n, float* a, float* y );

/*sine & cosine*/
void p_sincos_32f(int n, float* a, float* y, float* z );

/*hyperbolic Sine*/
void p_sinh_32f(int n, float* a, float* y );

/*tangent*/
void p_tan_32f(int n, float* a, float* y );

/*hyperbolic Tangent*/
void p_tanh_32f(int n, float* a, float* y );
```
 
**Reduction Operations**

``` c
/*sum*/
void p_sum_32f(int n, float* a, float* y  );

/*avarage of vector*/
void p_ave_32f(int n, float* a, float* y );

/*mean of vector*/
void p_mean_32f(int n, float* a, float* y );

/*find max value in a vector and index*/
void p_maxval_32f(int n, float* a, int* index, float* y );

/*find min value in a vector and index*/
void p_minval_32f(int n, float* a, int* index, float* y );

/*sum of product on vector*/
void p_sop_32f(int n, const float* a, const float* b, const float* c);

```

**Matrix Operations**
``` c

/*multiply two matrices*/
void p_matmul_32f(int m, int n, int k, float* a, float* b, float* c);

/*add two matrices*/
void p_matadd_32f(int m, int n, float* a, float* b, float* c);

/*subtract two matrices*/
void p_matsub_32f(int m, int n, float* a, float* b, float* c);

/*absolute difference of two matrices*/
void p_matabsdiff_32f(int m, int n, float* a, float* b, float* c);

/*absolute difference oftwo matrices*/
void p_matsqadd_32f(int m, int n, float* a, float* b, float* c);

/*element wise division*/
void p_matdiv_32f(int m, int n, float* a, float* b, float* y );

/*element wise square root*/
void p_matsqrt_32f(int m, int n, float* a, float* y );

/*element wise inverse square root*/
void p_matisqrt_32f(int m, int n, int k, float* a, float* y );

/*invert a matrix, gauss jordan, not singular, square, no error checking*/
void p_matinv_32f(int m, float* a, float* y);

/*transpose a matrix*/
void p_mattran_32f(int m, int k, float* a, float* c);

```



## DSP

**1D**
* p_acorr_32f()
* p_conf_32f()
* p_corr_32f()
* p_fir_32f()
* p_firdec_32f()
* p_firint_32f()
* p_firlat_32f()
* P_iir_32f()
* p_hist_32f()

**2D**
* p_conv2D_32f()
* p_acorr2D_32f()
* p_ave2D_32f()
* p_median2D_32f()
* p_sobel2D_32f()
* p_box2D_32f()
* p_canny2D_32f()
* p_harris2D_32f()
* p_gauss2D_32f()
* p_sad2D_32f()
* p_mad2D_32f()
* p_hist2D_32f()
* p_mag2D_32f()
* p_histeq2D_32f()
* p_minmax2D_32f()
* p_thresh2D_32f()
* p_scale2D_32f()
* p_lut2D_32f()
* p_memcpy2D_32f()

## FFT
* p_fftplan_32f()
* p_fftexec_32f()
* p_fftdestroy()


