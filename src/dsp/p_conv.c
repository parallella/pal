#include <pal.h>

/**

 * Computes the convolution of two vectors 'x' and 'h', and places the
 * results in vector 'r'.
 *
 * @param x       Pointer to input vector of size 'nr+nh-1' elements
 *
 * @param h       Pointer to 'nh' filter coefficients
 *
 * @param r       Output vector of size 'nr'
 *
 * @param nr      The number of output samples
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
void p_conv_f32(float *x, float *h, float *r, int nr, int nh,
		int p, p_team_t team)
{
	for (int i = nr; i >= nh; i--)
  	{
    		for (int j = nh; j >= 0; j--)
    		{
      			*r(i+j-nh) = *r(i+j-nh) + *x(i) * *h(j);
    		}
  	}
}
