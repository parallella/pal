#include <pal.h>

/**
 * A 3x3 box filter (aka a moving average filter)
 * Basically a convolution with a 3x3 coefficient matrix all set to ones.
 *
 *    |1 1 1|
 * M =|1 1 1| * 1/9
 *    |1 1 1|
 *
 * Notes: cols and rows must be 3 or more
 * @todo rows isn't being used, why?
 *
 * @param x    Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param rows Number of rows in input image
 *
 * @param cols Number of columns in input image
 *
 * @param r    Pointer to output image (size 'rows - 2' x 'cols - 2')
 *
 */

void p_box3x3_f32(const float *x, float *r, int rows, int cols)
{

    int ia, ja;    
    const float *px;
    float *pr;
    float r1avg, r2avg, r3avg;

    px = x;
    pr = r;

    const float *col_in;
    float *col_out;
    for (ja = cols - 2; ja != 0; ja--) {
        col_in = px;
        col_out = pr;

        r1avg = (*px++);
        r1avg += (*px++);
        r1avg += (*px++);
        px += cols - 3;

        r2avg = (*px++);
        r2avg += (*px++);
        r2avg += (*px++);
        px += cols - 3;

        ia = cols - 2;
        do {
            r3avg = (*px++);
            r3avg += (*px++);
            r3avg += (*px++);
            px += cols - 3;

            *pr = (r1avg + r2avg + r3avg) * M_DIV9;

            pr += cols - 2;
            r1avg = r2avg;
            r2avg = r3avg;
        } while (--ia);

        px = col_in + 1;
        pr = col_out + 1;
    }

    return;
}
