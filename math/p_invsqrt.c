/**
 *
 * Calculates the inverse square root of the input vector 'a'. 
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *       
 * @return      None
 *
 */
#include <math.h>
void p_invsqrt_32f (float* a, float* c, int n ){
    int i;
    for(i=0;i<n;i++){
	*(c+i)=1.0f / sqrtf(*(a+i));
    }
}
