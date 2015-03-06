#include <pal.h>

/**
 * Computes the raw full-length linear cross correlation of vectors x and y and
 * stores the result in vector r. Normalization is done outside this function.
 *
 * @param x   Pointer an input vector of floats of 'nx' elements
 *
 * @param y     Pointer an input vector of floats of 'ny' elements
 *
 * @param r     Pointer an output vector of floats of 'ny+ny-1' elements
 *
 * @param nx    Number of elements in vector 'x'
 *
 * @param ny    Number of elements in vector 'y'
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_xcorr_f32(float *x, float *y, float *r, int nx, int ny,
		 int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}
