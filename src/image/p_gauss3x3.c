#include <pal.h>

/**
 * A 3x3 gauss smoothing filter with the following convolution matrix
 *
 *    |1 2 1|
 * M =|2 4 2| * 1/16
 *    |1 2 1|
 *
 * Notes: cols must be a multiple of 2
 *
 * @param x    Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r    Pointer to output image
 *
 * @param rows Number of rows in input image
 *
 * @param cols Number of columns in input image
 *
 * @return     None
 *
 */

void p_gauss3x3_f32(const float *x, float *r, int rows, int cols)
{
    int i, j;
	int rm2 = rows - 2;
    int cm2 = cols - 2;
	int cj = 1 + 2 * cols;
	float P;
    const float *px;
    float *pr;

    px = x;
    pr = r;

    for (i = 0; i < rm2; i++) {
        for (j = 0; j < cm2; j++) {

            P = (*px++);
            P += (*px++) * 2;
            P += (*px);
            px += cm2;
            P += (*px++) * 2;
            P += (*px++) * 4;
            P += (*px) * 2;
            px += cm2;
            P += (*px++);
            P += (*px++) * 2;
            P += (*px);
            px -= cj;

            *pr = P * M_DIV16;
            pr++;
        }
        px = px + 2;
    }

    return;
}
