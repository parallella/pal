#include <pal.h>

/*
 * A 3x3 box filter (aka a moving average filter)
 * Basically a convolution with a 3x3 coefficient matrix all set to ones.
 *
 *    |1 1 1|
 * M =|1 1 1| * 1/9
 *    |1 1 1|
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

void p_box3x3_f32(float *x, float *r, int rows, int cols,
		  int p, p_team_t team)
{

    int ia, ja;
    float E;
    float *px, *pr;

    px = x;
    pr = r;

    for (ia = (rows - 2); ia != 0; ia--) {
        for (ja = (cols - 2); ja != 0; ja--) {
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
            E += (*px++);
            px += cols - 3;
            *pr = E * M_DIV9;
            px += 1 - 3 * cols; // advance mask matrix in one column.
            pr++;
        }
        px = px + 2; // advance pointer to the beginning of next row.
    }

    return;
}
