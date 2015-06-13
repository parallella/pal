/*
// FreeMat 
//--> x = [1, 2, 3, 4];
//--> h = [5,6,7];
//--> r = conv(x, h)
//r =
//  5 16 34 52 45 28 

// result from pal : 
   float x[4] = {1,2,3,4};
   float h[3] = {5,6,7} ;
   float r[10] = {100,100,100,100,100,100, 100, 100,100, 100};  
   p_conv_f32( x, h , r ,4 ,3);
   printf ("%f, %f, %f, %f, %f, %f, %f, %f", r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7]);
//100.000000, 105.000000, 116.000000, 134.000000, 152.000000, 145.000000, 128.000000, 100.000000
*/

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
void p_conv_f32(const float *x, const float *h, float *r,
                int nx, int nh, int p, p_team_t team)
{
    const float *xc = x;
    float *rx = r;
    int i,j ;
        for ( i = 0; i < nx+nh-1; i++) { *(rx++) = 0; } 
        rx = r ;
  	for ( i = 0; i < nx; i++) {
        float xv = *xc++;

  		for (j = 0; j < nh; j++) {
  			*(rx + j) += xv * *(h + j);	
  		}
        rx++;
  	}
}
