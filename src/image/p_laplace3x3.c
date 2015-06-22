#include <pal.h>

/**
 * A Laplace 3x3 convolution filter (m) with the Laplace operators defined as:
 *
 *        |  0 -1  0 |
 * LA3 =  | -1  4 -1 |
 *        |  0 -1  0 |
 *
 * Notes: cols must be a multiple of 2
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @return      None
 *
 */
void p_laplace3x3_f32(const float *x, float *r, int rows, int cols)
{

    int ia, ja;
    float LA;
    const float *px;
    float *pr;

    px = x;
    pr = r;

    for (ia = 1; ia <= (rows - 2); ia++) {
        for (ja = 1; ja <= (cols - 2); ja++) {
            LA = 0;
            px++;
            LA -= (*px++);
            px++;
            px += cols - 3;
            LA -= (*px++);
            LA += (*px++) * 4;
            LA -= (*px++);
            px += cols - 3;
            px++;
            LA -= (*px++);
            px++;
            px += cols - 3;
            *pr = LA;
            px += 1 - 3 * cols; // advance mask matrix in one column.
            pr++;
        }
        px = px + 2; // at the last column in a row, advance pointer to the
                     // beginning of next row.
                     //              pr = pr + 2;
    }

    return;
}
