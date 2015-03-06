#include <pal.h>

/**
 * Computes a cascaded IIR filter of 'nb' biquad sections using 32-bit
 * coefficients and 32-bit delay buffers. Each biquad section is implemented
 * using Direct-form II.
 *
 * @param x     Pointer an input vector with 'nx' elements
 *
 * @param h     Pointer to vector containing '5*nb' filter coefficient
 *
 * @param r     Pointer an output vector of floats of 'nr' elements
 *
 * @param nb    Number of biquads in filter
 *
 * @param nr    Size of input and output vectors
 * 
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      Returns 0 if successful
 *
 */

void p_iir_f32(float *x, float *h, float *r, int nb, int nr,
	       int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}
