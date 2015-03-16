#include <pal.h>

/*
 * A median 3x3 filter.
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
 * @return      None
 *
 */

void p_median3x3_f32(float *x, float *r, int rows, int cols, 
		     int p, p_team_t team)
{
    float buffer[9];
    float *px, *pr;
    int i, j, buffer_col;

    px = x;
    pr = r;

    for (i = 0; i < rows - 2; i++) {
        // fully filling first window
        buffer[0] = *px;
        buffer[1] = *(px + 1);
        buffer[2] = *(px + 2);

        buffer[3] = *(px + cols);
        buffer[4] = *(px + cols + 1);
        buffer[5] = *(px + cols + 2);

        buffer[6] = *(px + cols + cols);
        buffer[7] = *(px + cols + cols + 1);
        buffer[8] = *(px + cols + cols + 2);

        p_median_f32(buffer, pr, 9, 0, 0);
        pr++;
        px += 3;
        // other windows differ only by one column
        // so only one is exchanged
        for (j = 0; j < cols - 3; j++) {
            buffer_col = j % 3;
            buffer[buffer_col] = *px;
            buffer[buffer_col + 3] = *(px + cols);
            buffer[buffer_col + 6] = *(px + cols + cols);

            p_median_f32(buffer, pr, 9, 0, 0);
            pr++;
            px++;
        }
    }
}
