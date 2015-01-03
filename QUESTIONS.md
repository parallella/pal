Open equestions/assumptions:
==================================

* Q: Impose a minimum vector length of 4 for speed?  
  A: 

* Q: Trig functions range
  A: 0..2pi

* Q: Work with matrix objects or on arrays?  
  A:  

* Q: Work with explicit C99 native types?  
  A:  
  
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
 
* Q: Perfect API compatibility
  A: The ability to run the exact same code on multiple platforms is magic!
     Strive for MPI and POSIX same name no change compatibility.
     Build up with time

 *Q: What DSP/math API's where researched?
  A: SAL:     http://opensal.net/  
     VSIPL:   http://portals.omg.org/hpec/files/vsipl/software3.html  
     LIQUID:  http://liquidsdr.org/  
     NUMERIX: http://www.numerix-dsp.com/siglib.html  
     CMSIS:   http://www.keil.com/pack/doc/CMSIS/DSP/html/index.html
     OPENCV:  http://docs.opencv.org/
     OPENVX:  https://www.khronos.org/openvx/
     CUDA:    
     MKL:
     EIGEN:
     BLAS:
     FFTW:
   
*Q: Which POSIX starting point
  A: Bionic, newlib, uClibc?

  http://08926976467793358646.googlegroups.com/attach/0f8eba5ecb95c6f4/OVERVIEW.TXT?part=4&view=1&vt=ANaJVrFbDIqCdywFb24oT2VrarYJMy1EUpl-7AOO-klYKQuRSf-JFpCIR6UQfHpYdXO5PXx9b0syhIYYxpxVkkAs0rJgbC3jT_AVyrQaqvmzqDuB_rRuKBM



