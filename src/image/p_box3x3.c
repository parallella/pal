#include <pal.h>

/**
 * A 3x3 box filter (aka a moving average filter)
 * Basically a convolution with a 3x3 coefficient matrix all set to ones.
 *
 *    |1 1 1|
 * M =|1 1 1| * 1/9
 *    |1 1 1|
 *
 * Notes: cols must be a multiple of 2
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

void p_box3x3_f32(const float *x, float *r, int rows, int cols)
{

    int ia, ja;
    float E;
    const float *px;
    float *pr;

    px = x;
    pr = r + 1 + cols; // center of filter box

    for (ia = (rows - 2); ia; ia--) {
        for (ja = (cols - 2); ja; ja--) {
            E = 0;
            E += (*px++);
            E += (*px++);
            E += (*px++);
            px += cols - 3;
            E += (*px++);
            E += (*px++);
            E += (*px++);
            px += cols - 3;
            E += (*px++);
            E += (*px++);
            E += (*px);
            px += 1 - 2 * cols; // reposition pointer from bottom right to middle top cell
            
            *pr = E * M_DIV9;
            pr++;
        }
        px = px + 2; // advance pointer to the beginning of the next row.
        pr = pr + 3; // advance pointer to the second pixel of the next row.
    }

    return;
}
