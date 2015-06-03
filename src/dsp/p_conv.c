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
 * @param p       Number of processor to use (task parallelism)
 *
 * @param team    Team to work with 
 *
 * @return        None
 *
 */
void p_conv_f32(float *x, float *h, float *r, int nx, int nh, int p, p_team_t team)
{
    float *xc = x;
    float *rx = r;
  	for ( int i = 0; i < nx; i++) {
        float xv = *xc++;
        rx++;
  		for (int j = 0; j < nh; j++) {
  			rx[j] += xv * h[j];		
  		}
  	}
}
