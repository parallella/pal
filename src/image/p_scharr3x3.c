#include <pal.h>

/*
 * A Scharr 3x3 convolution filter with the following convolution matrix:
 *
 *       |  3  0 -3 |
 * Gx =  | 10  0 10 | * 1/?
 *       |  3  0 -3 |
 *
 *       |  3 10  3 |
 * Gy =  |  0  0  0 | * 1/?
 *       | -3 10 -3 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 *
 * Gradient Direction (theta) = atan2(Gy,Gx)
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
 */

void p_scharr3x3_f32(const float *x, float *r, int rows, int cols,
                     int p, p_team_t team)
{
}
