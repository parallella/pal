#include <pal.h>

/*
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 16×16 region pointed to in the reference image 'm'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to an 16x16 pixel array within the image.
 *
 * @param r     Result scalar
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */
void p_sad16x16_f32(float *x, float *m, float *r, int rows, int cols, 
                  int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}
