Open equestions/assumptions:
==================================

* Q: Impose a minimum vector length of 4 for speed?  
  A: 

* Q: Trig functions range
  A: 0..2pi

* Q: Work with matrix objects or on arrays?  
  A: Raw arrays as default

* Q: Work with explicit C99 native types for DSP library?  
  A:  Yes, no savings in hidden structures, except for types
  
* Q: Library vs API portability?  
  A: ??  

* Q: 32/64 bit portability  
  A:   

* Q: Argument order, "natural" or compatible with other the "other" library
  A: Natural order, except for when we want to implement exact API

* Q: Saturation/overflow policy
  A: GIGO

* Q: Include scatter gather in basic memcpy operations
  A: Yes, fundemental to DMAs, should be in hal
   
* Q: Include barrier?
  A: Yes, fundemental to all models, based on absolute address like mutex
 
* Q: Perfect API compatibility?
  A: The ability to run the exact same code on multiple platforms is magic!
     Strive for MPI and POSIX same name no change compatibility.

 *Q: What DSP/math API's where researched?
  A: SAL:     http://opensal.net/  
     VSIPL:   http://portals.omg.org/hpec/files/vsipl/software3.html  
     LIQUID:  http://liquidsdr.org/  
     NUMERIX: http://www.numerix-dsp.com/siglib.html  
     CMSIS:   http://www.keil.com/pack/doc/CMSIS/DSP/html/index.html
     OPENCV:  http://docs.opencv.org/
     OPENVX:  https://www.khronos.org/openvx/
     CUDA:    http://docs.nvidia.com/cuda/pdf/CUDA_C_Programming_Guide.pdf
     MKL:     
     EIGEN:
     BLAS:
     FFTW:
   
*Q: Which POSIX starting point
  A: Bionic, newlib, uClibc, other?

*Q: int dd or explicit structure type?
 A: file descriptor..

*Q: Should we have the ability to open device as stream, what does it mean?
 A: No, that is one level up.(fifo=memory+state)

*Q: Use argc/argv for passing variables?
 A: Make the argv of type void* args[], not just chars

*Q: NAN, denormal numbers in math
 A: pass through (gigo)

*Q: Error checking in library
 A: How about a "debug" #define flag that gets turned off in production.
    (would prefer not having it at all...)
 
*Q: Do we need a level of abstraction above the memory pointer?
 A: Yes

*Q: Do we still need a fast memcpy like interface?
 A: Yes, but it's niche for shared memory systems

*Q: What should the processor id be?
 A: A simple integer

*Q: At what level should we be doing work?
 A: At the team level

*Q: What memory oprations to support?
 A: host writing to epiphany registers-->p_write
    epiphany writing to other epiphany-->p_write
    host to shared memory-->p_write, assign alloc to a team

*Q: Assume contiguos memory in all buffers?
 A: Yes

*Q: Support no holds bar memory direct read/writes to addresses?
 A: 1. declare a team that holds all the cores in the system
    2. create a 2D array of pal_mem_t structs, each one 32KB 
          
*Q: Is "remote malloc" on processor 'n' a bad idea?
 A: No, but implementation might be tricky:-) Can it work in a request/signal
    fashion?

*Q: Make all data movement calls non-blocking?
 A: Yes, you can always add the blocking part in wrapper above.
    Non-blocking is the lego block.

*Q: pal_* or p_*
 A: Clarity vs brevity?

*Q: The building blocks.
 A: 1. A current processor "X", ie "me".
    2. Other processors "Y(s)", ie "not me"
    3. A set of named memory buffers belonging to processor "X", AND "Y"
    4  A team of N processors, at least one X and one Y
    5. A startup program on processor X, ie "root processor".

*Q: Make data structures completely opaque?
 A: Yes (a processor could be almost anything)

*Q: Return value as return argument or in argument list?
 A: Prefer simple functions with short argument lists.
    We know what functional programming would suggest..

*Q: Typedefs for some structs
 A: Yes, because the intent is to make these completely opaque
    
*Q: Include events in argument list?
 A: No. Complicated, can't think of a clean solution

*Q: Include event information in memory/team objects
 A: Read/write ordering and "done" is too tricky for everyone.

*Q: Argument ordering in functions: inputs, flags, otput
 A: Having flags last dies make some sense

*Q: Argument passing to run
 A: Enable passing arguments in "main style" and function-kernel style.     
    Contract between caller/calle program. Short cuts allowed

*Q: Is it practical to run the same executable on bare metal and in Linux?
 A: 

*Q: Use of "node" to represent a processor?
 A: The reason I chose this name was to abstract away as much as possible
    the biases people have. The word "core" has been completely bastardized
    by the indistry and spans anything from a multiply unit up to a CPU
    running Linux. The word processor could work, but also carries with it
    a heavy history and suggests a microprocessor. Node is non-descript
    which is a good thing. I also like that a node is often represented
    by a dot, which ironically is what compute hardware has become. Programmers
    should think about memory, dots, and lines when architecting algorihtms
    and programs. Anything beyond that is too much mental overhead.

*Q: Is there a need for a new memory object?
 A: Yes, memory is not the same thing as a file and files are not 
    appropriate as something to work with when doing HPC. We need very
    fast and direct manipulations of array data.

*Q: Make ssize_t a first class citizen for memory manipulation
 A: Yes, we can no longer assume that all writes succeed. The faulty domain
    of clusters is making it down to chips. 

*Q: Why not make the PAL memory objects true file descriptors?
 A: 

*Q: Math arg name output, change to 'res' for clarity
 A: 

*Q: Include consts for input pointers to functions, any other keyword?
 A: 



