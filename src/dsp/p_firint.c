#include <pal.h>

/**
 * Computes a interpolating FIR filter (direct-form) on input data 'x' using
 * coefficient stored in 'h' and places result in 'r'. This function
 * retains the address of the delay filter memory containing the previous
 * delayed values to allow consecutive processing of blocks.
 *
 * @param x       Pointer to input vector of 'nx' elements
 *
 * @param h       Pointer to 'nh' filter coefficients
 *
 * @param r       Output vector of size 'nx*ifactor'
 *
 * @param nx      The number of input samples

 * @param nh      The number of coefficients of the filter
 *
 * @param ifactor Interpolation factor. 'ifactor' output samples produced
 *                for every 1 input sample
 *
 * @param p       Number of processor to use (task parallelism)
 *
 * @param team    Team to work with 
 *
 * @return        None
 *
 */

void p_firint_f32(const float *x, const float *h, float *r, int nx, int nh,
                  int ifactor, int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}
