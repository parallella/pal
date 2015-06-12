#include <pal.h>

/*
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 16×16 region pointed to in the reference image 'm'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to a 16x16 sub image 
 *
 * @param r     Result scalar
 *
 * @param cols  Number of columns in input image
 *
 * @return      None
 *
 */
void p_sad16x16_f32(const float *x, float *m, float *r, int cols)
{

     /*pseudo code
       for (i = 0; i < 16; i++)
         for (j = 0; j < 16; j++)
          sad += abs(m[j+i*16] - x[j+i*cols]);
       return sad;

     */

}
