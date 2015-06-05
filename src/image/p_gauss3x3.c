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
 * @param r    Pointer to output image
 *
 * @param rows Number of rows in input image
 *
 * @param cols Number of columns in input image
 *
 * @param p    Number of processor to use (task parallelism)
 *
 * @param team Team to work with 
 *
 * @return     None
 *
 */

void p_gauss3x3_f32(const float *x, float *r, int rows, int cols,
                    int p, p_team_t team)
{
    int i, j;
    const float *px;
    float *pr;
    float P;

    px = x;
    pr = r;

    for (i = 1; i < (cols - 1); i++) {
        for (j = 1; j < (rows - 1); j++) {
            P = 0;
            P += (*px++);
            P += (*px++) * 2;
            P += (*px++);
            px += cols - 3;
            P += (*px++) * 2;
            P += (*px++) * 4;
            P += (*px++) * 2;
            px += cols - 3;
            P += (*px++);
            P += (*px++) * 2;
            P += (*px++);
            px -= 2 + 2 * cols;
            *pr = P * M_DIV16;
            pr++;
        }
        px = px + 2;
    }

    return;
}
