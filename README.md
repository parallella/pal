PAL: The Parallel Architectures Library
========================================

The Parallel Architectures Library (PAL) is a free and open-source C-libary written for the kind of tiny programmable CPUs that can be found in embedded applications and massively parallel processor architectures.

We have found three main issues with existing solutions that we will try to address with the PAL library.

* Most IPC code was written for "large" processors and is not suitable for tiny processors
* Free and open source APIs (like math.h) is far too inefficient for parallel architectures (no vectors)
* Hardware vendor provided math and DSP libraries come with proprietary license restrictions

The design goals for the library are as follows:

* Fast (assumes program and data resides in local cache/scratchpad)
* Open (permissive open source license)
* Small (because every bit counts for embedded processors)
* Scalable (should lend itself to vector and task parallelism)
* Portable (should be useful for different ISAs, with and without vector extensions, 32/64 bit)

If you think we missed some important library that can be reused, please do let us know ASAP!  We would love nothing better than finding out that this work has already been done.

## PAL Components
``` c
* HAL     A universal hardware abstraction layer
* IPC     A "complete: set of parallel programming primitives (POSIX/SYSCALL COMPLIANT???)
* MATH    Vectorized math library
* DSP     Vectorized DSP library
* FFT     Optimized FFT library
```

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

Memory Management (global addresse map):
* p_read()
* p_write()
* p_memcpy()
* p_memcpy_async()
* p_memalloc()
* p_memfree()
* p_memptr()

Program Execution Management (workgroup):
* p_devctl()
* p_init()
* p_open()
* p_close()
* p_finalize()
* p_load()
* p_exec()
* p_barrier()

Hardware Management (local core):
* p_setfreq()
* p_setvolt()
* p_reset()
* p_timer_set()
* p_timer_get()
* p_timer_start()
* p_timer_stop()
* p_irq_mask()
* p_irq_set()
* p_irq_mask()


## IPC

Threads:
* p_attr_init()
* p_attr_destroy()
* p_attr_setdetachstate()
* p_attr_setdevice()
* p_attr_setinit()
* p_create()
* p_ncreate()
* p_join()

Mutex: 
* p_mutex_attr_init()
* p_mutex_atr_destroy()
* p_mutex_attr_setdevice()
* p_mutex_init()
* p_mutex_destroy()
* p_mutex_lock()
* p_mutex_unlock()
* p_mutex_trylock()

Conditionals:
* p_cond_attr_init()
* p_cond_attr_destroy()
* p_cond_attr_setdevice()
* p_cond_init()
* p_cond_destroy()
* p_wait()

Atomics:
* p_atomic_init()
* p_atomic_exchange()
* p_atomic_compare_exchange()
* p_atomic_fetch_add()
* p_atomic_fetch_sub()
* p_atomic_fetch_or()
* p_atomic_fetch_xor()
* p_atomic_fetch_and()
* p_atomic_fence()

Sockets:
* p_accept()
* p_bind()
* p_connect()
* p_listen()
* p_recv()
* p_recvfrom()
* p_recvmsg()
* p_send()
* p_sendmsg()
* p_sendto()
* p_socket()


## MATH
* p_itof()
* p_ftoi()
* p_itod()
* p_dtoi()
* p_acos_32f()
* p_acosh_32f()
* p_asin_32f()
* p_atan_32f()
* p_atan2_32f()
* p_cbrt_32f()
* p_cos_32f()
* p_cosh_32f()
* p_exp_32f()
* p_icbrt_32f()
* p_ln_32f()
* p_log10_32f()
* p_pow_32f()
* p_sin_32f()
* p_sincos_32f()
* p_sin_32f()
* p_tan_32f()
* p_tanh_32f()
* p_sum_32f()
* p_ave_32f()
* p_mean_32f()
* p_maxval_32f()
* p_minval_32f()
* p_sop_32f()
* p_matmul_32f()
* p_matadd_32f()
* p_matacc_32f()
* p_mataccw_32f()
* p_matand_32f()
* p_matxor_32f()
* p_mator_32f()
* p_matmac_32f()
* p_mattran_32f()
* p_matdiv_32f()
* p_matsqrt_32f()
* p_matisqrt_32f()
* p_matinv_32f()
* p_matabsdiff_32f()
* p_matsqadd_32f()


## DSP
* p_acorr_32f()
* p_conf_32f()
* p_corr_32f()
* p_fir_32f()
* p_firdec_32f()
* p_firint_32f()
* p_firlat_32f()
* P_iir_32f()
* p_hist_32f()
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


