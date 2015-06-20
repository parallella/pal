#include <pal.h>

/*
 * This function returns a scalar sum of the absolute differences between the
 * source block 'x' and an 16×16 region pointed to in the reference image 'img'.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param img   Pointer to a 16x16 sub image 
 *
 * @param r     Result scalar
 *
 * @param cols  Number of columns in input image
 *
 * @param rows  Number of rows in input image
 *
 * @return      None
 *
 */

void p_sad16x16_f32(const float *x, const float *img, float *r, int rows, int cols)
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
    for (i=0 ; i< cols-16+1 ; i++){
        for(j=0 ; j< rows-16+1; j++){
            sum = 0;
            px = x + (i * cols + j);
            pk = img ;
            for(k=0; k<16 ; k++){
                for(l=0; l<16 ; l++){
                    tmp.f = *(pk++) - *(px++);
                    tmp.u &= 0x7FFFFFFF ;
                    sum += tmp.f ;
                }
            px +=  cols - 16 ; 
            }
            *(pr++) = sum ;
        }
    }
}
