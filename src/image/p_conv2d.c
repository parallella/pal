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
                  int mrows, int mcols)

{
    int i, j, ki, kj;
    float P;
    const float *px, *pm;
    float *pr;

    px = x;
    pr = r;

    for (i = 0; i < rows - mrows+1  ; i++) {
        for (j = 0; j < cols - mcols+1 ; j++) {
            P = 0.0f;
            pm = m+(mcols * mrows)-1;
            for (ki=0 ; ki< mrows ; ki++){
                for (kj=0 ; kj< mcols ; kj++){
                    P+= (*px++)* (*pm--) ;
		}
            px += cols - mcols;
            }
            px -= (mrows * cols) -1 ;
            *(pr++) = P;
        }
    px+=mcols-1 ;
    }
}
