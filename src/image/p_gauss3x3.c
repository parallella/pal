#include <pal.h>

/*
 * A 3x3 gauss smoothing filter with the following convolution matrix
 *
 *    |1 2 1|
 * M =|2 4 2| * 1/16
 *    |1 2 1|
 *
 * @param x    Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param rows Number of rows in input image
 *
 * @param cols Number of columns in input image
 *
 * @param r    Pointer to output image
 *
 */

void p_gauss3x3_f32(float *x, int rows, int cols, float *r) {}
