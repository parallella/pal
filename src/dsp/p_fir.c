#include <pal.h>

/**
 * Computes a FIR filter (direct-form) on input data in vector 'x' using the
 * coefficients stored in vector 'h'. This function maintains the array
 * 'dbuf' containing the previous delayed input values to allow
 * consecutive processing of input data blocks.
 *
 * @param x     Pointer to input vector of 'n' elements
 *
 * @param h     Pointer to filter coefficients.
 *
 * @param r     Pointer to result vector
 *
 * @param nx    The number of input samples
 *
 * @param nh    The number of coefficients of the filter.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_fir_f32(float *x, float *h, float *r, int nx, int nh,
	       int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}

