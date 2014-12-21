PAL: The Parallel Architectures Library
==================================

The Parallel Architectures Library (PAL) is a free and open-source libary written in C designed for minimalist processor architectures . The initial target for the libary is the Epiphany, but the library should be applicable to a wide range of chips and architectures. 

## Design criteria
* Fast  (..when data and program fits in local cache/SRAM)
* Open (permissive open source license)
* Dense (to fit as much as possible into 32KB of local memory)
* Scalable (should lend itself to vector and task parallelism)
* Portable (should be useful for different ISAs, with and without vector extensions, 32/64 bit)

To meet all of these goals, we had to adhere to the garbage in garbage out philosophy.
Examples of such tradeoffs include limiting the range, cutting out corner cases, and doing away with error checking.

## Components

### HAL
Hardware abstraction layer (dma programming, counters, interrupts, memcpy)

### POSIX
A subset of POSIX functions needed for parallel programming, including support for mutexes, queues, signalling

### IPC
Support for functions not anticipated by the POSIX, including memory ordering syncronization and atomic functions

### MATH
A vectorized math library
(sin, cos, basic vector math operations, ...)

### DSP
A vectorized DSP library
(fft, fir/irr filters, median, gaussian histogram, sad, etc)

### GEMS
programming tricks passed down through generations

  
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

##Coding style 

* C99
* "K&R coding style, 4 spaces for tabs"

##Usage examples

TBD

##Contributing
PAl is an ambitious project and really neeeds all the help it can get. If you have anything to contribute, please do!! Istructions for get started can be found [HERE](CONTRIBUTING.md). 

##Licensing
PAL is licensed under the Apache License, Version 2.0. See LICENSE for full license text.

