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
 * @param dbuf    Pointer to delay buffer of length (nh/ifactor) + 1
 *
 * @param nx      The number of input samples

 * @param nh      The number of coefficients of the filter
 *
 * @param ifactor Interpolation factor. 'ifactor' output samples produced
 *                for every 1 input sample
 *
 * @param r       Output vector of size 'nx*ifactor'
 *
 * @return        None
 *
 */

void p_firint_32f(float *x, float *h, float *dbuf, int nx, int nh, int ifactor,
                  float *r)
{

    /*PLACE CODE HERE*/
}
