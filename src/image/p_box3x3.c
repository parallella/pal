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
    pr = r;

    // Process first row

    // Calculate the first sum
    sum = (*px) + (*(px+1)) + (*(px+2));
    // The sums from the first row are only used for a single row of the output.
    *pr = sum;
    pr++;
    for (j = 3; j < cols; j++) {
        // All subsequent sums are calculated by removing the leftmost
        // value, and adding the next.
        sum += (*(px+3)) - (*px);
        *pr = sum;
        px++;
        pr++;
    }
    px += 3; // Advance input pointer to beginning of next row.
    pr = r;

    // Process second row

    sum = (*px) + (*(px+1)) + (*(px+2));
    // The second row's sums are added to the first row's, and used to
    // initialize a new output row.
    *pr += sum;
    *(pr+cols-2) = sum;
    pr++;
    for (j = 3; j < cols; j++) {
        sum += (*(px+3)) - (*px);
        *pr += sum;
        *(pr+cols-2) = sum;
        px++;
        pr++;
    }
    px += 3; // Advance input pointer to beginning of next row.

    // Process (rows - 4) middle rows

    for (i = 4; i < rows; i++) {
        // These rows are the final inclusion to the output row from two
        // iterations ago (so the final division is applied). They are also
        // added to the previous row's output and used to initialize the
        // next row.
        sum = (*px) + (*(px+1)) + (*(px+2));
        *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
        *pr += sum;
        *(pr+cols-2) = sum;
        pr++;
        for (j = 3; j < cols; j++) {
            sum += (*(px+3)) - (*px);
            *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
            *pr += sum;
            *(pr+cols-2) = sum;
            px++;
            pr++;
        }
        px += 3; // Advance input pointer to beginning of next row.
    }

    // Process second-to-last row

    sum = (*px) + (*(px+1)) + (*(px+2));
    *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
    *pr += sum;
    pr++;
    for (j = 3; j < cols; j++) {
        sum += (*(px+3)) - (*px);
        *(pr-cols+2) = (*(pr-cols+2) + sum) * M_DIV9;
        *pr += sum;
        px++;
        pr++;
    }
    px += 3; // Advance input pointer to beginning of next row.
    pr -= cols-2; // Reset output pointer to the previous row.

    // Process last row

    sum = (*px) + (*(px+1)) + (*(px+2));
    *pr = (*(pr) + sum) * M_DIV9;
    pr++;
    for (j = 3; j < cols; j++) {
        sum += (*(px+3)) - (*px);
        *pr = (*(pr) + sum) * M_DIV9;
        px++;
        pr++;
    }
}
