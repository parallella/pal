#include <pal.h>

/**
 *
 * Computes auto-correlation of the vector 'x' and the first 'nr' points of the
 * positive half of the full correlation result in 'r'.
 *
 * @param x     Pointer to an input vector of floats of 'nx' elements
 *
 * @param r     Pointer an output vector of floats of 'nr' elements
 *
 * @param nx    Size of input vector
 *
 * @param nr    Size of output vector
 * 
 * @return      None
 *
 */

void p_acorr_f32(const float *x, float *r, int nx, int nr)
{
  int c_offset;
  int index;
  const float* x_off;
  const float* x_p;
  const float* x_po;

  x_off = x;
  for ( c_offset = 0; c_offset < nr; c_offset++ ) {
    *r = 0;
    x_p = x;
    x_po = x_off;
    for ( index = 0; index < nx - c_offset; index++ ){
      *r = *r + *x_p++ * *x_po++;
    }
    r++;
    x_off++;
  }
}
