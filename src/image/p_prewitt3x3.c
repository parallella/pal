#include <pal.h>

/*
 * A Prewitt 3x3 convolution filter (m) with the Prewitt operators defined as:
 *
 *       | -1  0  1 |
 * Gx =  | -1  0  1 | * 1/6
 *       | -1  0  1 |
 *
 *       | -1 -1 -1 |
 * Gy =  |  0  0  0 | * 1/6
 *       |  1  1  1 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 *
 * Gradient Direction (theta) = atan2(Gy,Gx)
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
void p_prewitt3x3_f32(float *x,  float *r, int rows, int cols,
		       int p, p_team_t team)
{

    int ia, ja;
    float Gx, Gy;
    float *px, *pr;

    px = x;
    pr = r;

    for (ia = 1; ia <= (rows - 2); ia++) {
        for (ja = 1; ja <= (cols - 2); ja++) {
            // Rewrite this code to be fast AND readable!
            Gx = Gy = 0;
            Gx -= (*px);
            Gy -= (*px++);
            Gy -= (*px++);
            Gx += (*px);
            Gy -= (*px++);
            px += cols - 3;
            Gx -= (*px);
            px++;
            px++;
            Gx += (*px);
            px++;
            px += cols - 3;
            Gx -= (*px);
            Gy += (*px++);
            Gy += (*px++);
            Gx += (*px);
            Gy += (*px++);
            px += cols - 3;
            *pr = sqrtf(Gx * Gx + Gy * Gy) * M_DIV6;
            px += 1 - 3 * cols; // advance mask matrix in one column.
            pr++;
        }
        px = px + 2; // at the last column in a row, advance pointer to the
                     // beginning of next row.
    }

    return;
}
