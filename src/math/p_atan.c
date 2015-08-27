

/*
 *
 * Calculates inverse tangent (arc tangent). 
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

void p_atan_f32(const float *a, float *c, int n)
{
 int i;int sign; float ff; float x;
 for ( i = 0; i < n; i++)
 {
 const float *pa = (a+i);
       float *pc = (c+i);
 ff = *pa;
 if (ff >= 0) sign = 1.0; else sign = -1.0;       // store sign of input variable
 if (ff < 0) x = ff * (-1.0f); else x = ff;       // this is equivalent of x=abs(a)

        float  z = 3.141592654f/4.0f
   +(x-1)/(x+1)*(+0.9999993329f
   +(x-1)/(x+1)*(-0.3332985605f*(x-1)/(x+1)
   +(x-1)/(x+1)*(+0.1994653599f*(x-1)/(x+1)*(x-1)/(x+1)
   +(x-1)/(x+1)*(-0.1390853351f*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)
   +(x-1)/(x+1)*(+0.0964200441f*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)
   +(x-1)/(x+1)*(-0.0559098861f*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)
   +(x-1)/(x+1)*(+0.0218612288f*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)
   +(x-1)/(x+1)*(-0.0040540580f*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)*(x-1)/(x+1)))))))));
       *pc = sign*z;
  }
}
