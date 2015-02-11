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
 * @param dbuf  Pointer to delay buffer of length nh+2
 *
 * @param nh    The number of coefficients of the filter.
 *
 * @param nx    The number of input samples
 *
 * @param r     Pointer to result vector
 *
 * @return      None
 *
 */

void p_fir_32f(const float *x, const float *h, float *dbuf, int nx, int nh,
               float *r)
{

    /*PLACE CODE HERE*/
}
