#include <pal.h>

/*
 * A median 3x3 filter.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r     Pointer to output image
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

void p_median3x3_f32(float *x, float *r, int rows, int cols, 
		     int p, p_team_t team)
{

    /*PLACE CODE HERE*/
}
