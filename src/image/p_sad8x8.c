#include <pal.h>

/*
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 8×8 sub image 'm'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to an 8x8 sub image
 *
 * @param r     Result scalar
 *
 * @param cols  Number of columns in input image
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 */

void p_sad8x8_f32(float *x, float *m, float *r, int cols, 
		  int p, p_team_t team)
{
      /*pseudo code
       for (i = 0; i < 8; i++)
         for (j = 0; j < 8; j++)
          sad += abs(m[j+i*8] - x[j+i*cols]);
       return sad;

     */

}
