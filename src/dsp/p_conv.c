#include <pal.h>

/**

 * Computes the convolution of two vectors 'x' and 'h', and places the
 * results in vector 'r'.
 *
 * @param x       Pointer to input vector of size 'nr' elements
 *
 * @param h       Pointer to 'nh' filter coefficients
 *
 * @param r       Output vector of size 'nr+nh-1'
 *
 * @param nx      The number of input samples
 *
 * @param nh      The number of coefficients of the filter
 *
 * @return        None
 *
 */
void p_conv_f32(const float *x, const float *h, float *r, int nx, int nh)
{
  int i,n;
  float xv = *x;

  for(i = 0; i < nh; i++) r[i] = xv * h[i];

  for(n = 1; n < nx; n++){
    xv = *(++x); r++;
    for(i = 0; i < nh-1; i++) r[i] += xv * h[i];
    r[i] = xv * h[i];
  }
}
