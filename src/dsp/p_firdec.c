#include <pal.h>

/**
 * Computes a decimating FIR filter (direct-form) on input data 'x' using
 * coefficient stored in 'h' and places result in 'r'. This function
 * retains the address of the delay filter memory containing the previous
 * delayed values to allow consecutive processing of blocks.
 *
 * @param x     Pointer to input vector of 'n' elements
 *
 * @param h     Pointer to filter coefficients.
 *
 * @param dbuf  Pointer to delay buffer of length nh+2
 *
 * @param nh    The number of coefficients of the filter.
 *
 * @param nx    The number of input samples
 *
 * @param df    Decimation factor. (1 output sample per 'd' input samples)
 *
 * @param r     Pointer to result vector of size nx/d.
 *
 * @return      None
 *
 */
void p_firdec_f32(const float *x, const float *h, float *dbuf, int nx, int nh,
                  int df, float *r)
{

    /*PLACE CODE HERE*/
}
