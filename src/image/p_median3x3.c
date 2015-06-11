#include <string.h>
#include <pal.h>

inline void sort(float *ptr, int lhs, int rhs)
{	
    if (ptr[lhs]>ptr[rhs]) {
        float tmp = ptr[rhs];
        ptr[rhs] = ptr[lhs];
        ptr[lhs] = tmp;
    }
}

// median of a 9 element vector
void median9(const float *a, float *c, int p, p_team_t team)
{
    float copy_a[9];
    memcpy(copy_a, a, sizeof(float) * 9);
    
    // minimum number of comparisons for median of 9 elements
    sort(copy_a, 1, 2);
    sort(copy_a, 4, 5);
    sort(copy_a, 7, 8);
    sort(copy_a, 0, 1);
    sort(copy_a, 3, 4);
    sort(copy_a, 6, 7);
    sort(copy_a, 1, 2);
    sort(copy_a, 4, 5);
    sort(copy_a, 7, 8);
    sort(copy_a, 0, 3);
    sort(copy_a, 5, 8);
    sort(copy_a, 4, 7);
    sort(copy_a, 3, 6);
    sort(copy_a, 1, 4);
    sort(copy_a, 2, 5);
    sort(copy_a, 3, 7);
    sort(copy_a, 4, 2);
    sort(copy_a, 6, 4);
    sort(copy_a, 4, 2);

    *c = copy_a[4];
}

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

void p_median3x3_f32(const float *x, float *r, int rows, int cols,
                     int p, p_team_t team)
{
    float buffer[9];
    const float *px;
    float *pr;
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

        median9(buffer, pr, 0, 0);
        pr++;
        px += 3;
        // other windows differ only by one column
        // so only one is exchanged
        for (j = 0; j < cols - 3; j++) {
            buffer_col = j % 3;
            buffer[buffer_col] = *px;
            buffer[buffer_col + 3] = *(px + cols);
            buffer[buffer_col + 6] = *(px + cols + cols);

            median9(buffer, pr, 0, 0);
            pr++;
            px++;
        }
    }
}
