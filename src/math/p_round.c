#include <pal.h>

/*
 *
 * Compute the round of a
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

void p_round_f32(const float *a, float *c, int n)
{
    int i;int sign; float ff; float x;
    for ( i = 0; i < n; i++)
      {
       const float *pa = (a+i);
             float *pc = (c+i);
       ff = *pa;
       if (ff >= 0) sign = 1.0; else sign = -1.0;       // store sign of input variable
       if (ff < 0) x = ff * (-1.0f); else x = ff;       // this is equivalent of x=abs(a)

  if (x >= 0x1.0p23)  *pc = sign*x;
  *pc = sign*((float) (unsigned int) (x + 0.49999997f));
      }
}