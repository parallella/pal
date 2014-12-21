#include <complex.h>

/*
 * FFT Routines (power of 2 only to start with)
 * (64,128,256,512,1024,2048,4096,8192,16384,32768,65536)
 *
 */

/*Set up FFT plan*/
p_fftplan_32f(xx, int nx, int ny, int nz, int flags);

/*Execute FFT*/
p_fftexec_32f(xx, float *datain, float *dataout, int flags);

/*Tear down FFT plan*/
p_fftdestroy(xx);
