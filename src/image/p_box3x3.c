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

void p_box3x3_f32(const float *x, float *r, int rows, int cols)
{

    int i, j;
    float sum;
    const float *px;
    float *pr;

    px = x;
    // Initialize the output pointer to the (-1, 0) coordinate
    pr = r - cols + 2;

    for (i = 0; i < rows; i++) {
        // The first sum is calculated directly.
        sum = (*px) + (*(px+1)) + (*(px+2));
        if (i >= 2) {
            // The first two input rows do not correspond to the bottom of any
            // pixel filter, so skip this logic for those rows.
            *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
        }
        if (i != 0 && i != rows-1) {
            // The first and last input rows do not correspond to the middle
            // of any pixel filter, so skip this logic for those rows.
            *pr += sum;
        }
        if (i < rows - 2) {
            // The last two input rows do not correspond to the top of any
            // pixel filter, so skip this logic for those rows.
            *(pr+cols-2) = sum;
        }
        pr++;
        for (j = 3; j < cols; j++) {
            // All subsequent sums (in the same row) are calculated by
            // removing the leftmost value, and adding the value to the
            // immediate right of the currently included values.
            sum += (*(px+3)) - (*px);
            if (i >= 2) {
                *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
            }
            if (i != 0 && i != rows-1) {
                *pr += sum;
            }
            if (i < rows - 2) {
                *(pr+cols-2) = sum;
            }
            px++;
            pr++;
        }
        px += 3; // Advance input pointer to beginning of next row.
    }
}
