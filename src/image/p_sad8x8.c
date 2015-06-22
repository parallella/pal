#include <pal.h>

/**
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 8×8 sub image 'm'.
 *
 * Notes: cols must be a multiple of 2
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to an 8x8 sub image
 *
 * @param r     Result scalar
 *
 * @param cols  Number of columns in input image
 *
 * @return      None
 */

void p_sad8x8_f32(const float *x, float *m, float *r, int cols)
{
      /*pseudo code
       for (i = 0; i < 8; i++)
         for (j = 0; j < 8; j++)
          sad += abs(m[j+i*8] - x[j+i*cols]);
       return sad;

     */

}
