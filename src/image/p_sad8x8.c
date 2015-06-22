#include <pal.h>

/**
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 8×8 sub image 'img'.
 *
 * Notes: cols must be a multiple of 2
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param img   Pointer to an 8x8 sub image
 *
 * @param r     Result scalar
 *
 * @param cols  Number of columns in input image
 *
 * @param rows  Number of rows in input image
 *
 * @return      None
 */

void p_sad8x8_f32(const float *x, const float *img, float *r, int rows, int cols)
{
    union {
        float f;
        uint32_t u;
    } tmp;
    int i,j ;
    int k,l ;
    const float *px, *pk ;
    float *pr=r ; 
    float sum ;
    for (i=0 ; i< cols-8+1 ; i++){
        for(j=0 ; j< rows-8+1; j++){
            sum = 0 ;
            px = x + (i * cols + j);
            pk = img ;
            for(k=0; k<8 ; k++){
                for(l=0; l<8 ; l++){
                    tmp.f = *(pk++) - *(px++);
                    tmp.u &= 0x7FFFFFFF ;
                    sum += tmp.f ;
                }
            px +=  cols - 8 ; 
            }
            *(pr++) = sum ;
        }
    }
}
