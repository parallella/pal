#include <pal.h>

/** Convolution on input image 'x' with a square kernel 'm' of size 'msize'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param m     Pointer to convlution kernel
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 * @param msize Size of convolution kernel
 *
 * @return      None
 *
 */

void p_conv2d_f32(const float *x, float *m, float *r, int rows, int cols,
                  int msize)

{
    int i, j, k;
    float P, part;
    const float *px, *pm;
    float *pr;

    px = x;
    pm = m;
    pr = r;

    for (i = msize * 0.5; i < (rows - msize * 0.5); i++) {
        for (j = msize * 0.5; j < (cols - msize * 0.5); j++) {
            P = 0.0f;
            pm = m;
            for (k = 0; k < msize; k++) {
                p_dot_f32(px, pm, &part, msize);
                P += part;
                px += cols;
                pm += msize;
            }
            *pr = P;
            pr++;
            // move image pointer one index forward compared to
            // the position from before `for` loop
            px += 1 - msize * cols;
        }
        // move image pointer to the beginning of line
        // beneath the current line
        px += (int)(msize * 0.5) * 2;
    }
}
