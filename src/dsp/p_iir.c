#include <pal.h>

/**
 * Computes a cascaded IIR filter of 'nb' biquad sections using 32-bit
 * coefficients and 32-bit delay buffers. Each biquad section is implemented
 * using Direct-form II.
 *
 * @param x    Pointer an input vector with 'nx' elements
 *
 * @param h    Pointer to vector containing '5*nb' filter coefficient
 *
 * @param dbuf Pointer to delay line buffer
 *             Each biquad has 3 consecutive delay line elements
 *
 * @param nb   Number of biquads in filter
 *
 * @param nr   Size of input and output vectors
 *
 * @param r    Pointer an output vector of floats of 'nr' elements
 *
 * @return     Returns 0 if successful
 *
 */

void p_iir_32f(const float *x, const float *h, float *dbuf, int nb, int nr,
               float *r)
{

    /*PLACE CODE HERE*/
}
