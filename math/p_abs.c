/**
 *
 * Compute the absolute value of the vector 'a'. 
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to result vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *       
 * @return      None
 *
 */
#include <math.h>
void p_abs_32f ( const float* a, float* c, int n ){

    int i;
    for(i=0;i<n;i++){
	if(*(a+i)<0){
	    *(c+i) = -*(a+i);
	}
	else{
	    *(c+i) = *(a+i); 
	}
    }    
}
