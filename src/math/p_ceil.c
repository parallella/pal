#include <pal.h>
/**
*
* Returns round up of input vector a.
*
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

void p_ceil_f32(const float *a, float *c, int n)
{
    int i; float x;
    for ( i = 0; i < n; i++)
      {
       const float *pa = (a+i);
             float *pc = (c+i);
       x = *pa;
       int j= (int) x;
       if (j < x) j++;
       *pc = j;
      }
}
