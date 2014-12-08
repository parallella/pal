PAL: The Parallel Architectures Library
==================================

The Parallel Architectures Library (PAL) is a free and open-source libary written in C designed for "memory challenged" architectures in mind. The initial target for the libary is the Epiphany, but the library should be applicable to a wide range of chips and architectures. 

* Math library (sin, cos, basic vector math operations, ...)
* DSP library (fft, 1D-filters, sobel filter, median, gaussian histogram, sad,..)
* Hardware abstraction layer (dma programming, counters, interrupts)
* POSIX (threads,queues)

## Design criteria
* Fast  (when data and program fits in local cache/SRAM)
* Open (duh!)
* Dense (to fit as much as possible into 32KB of local memory)
* Simple (no need to reinvent the wheel)
* Scalable (should lend itself to vector and task parallelism at upper layer)
* Portable (should be useful for different ISAs, with and without vector extensions, 32/64 bit)
 
To meet all of these goals, we had to adhere to the garbage in garbage out philosophy.
Examples of such tradeoffs include limiting the range, cutting out corner cases, and doing away with error checking.

##Coding style 

* C99
* "K&R coding style, 4 spaces for tabs"

##Usage examples

TBD

##Contributing
PAl is an ambitious project and really neeeds all the help it can get. If you have anything to contribute, please do!! Here are the instructions to get started [HERE](CONTRIBUTING.md). 

##Licensing
PAL is licensed under the Apache License, Version 2.0. See LICENSE for full license text.

