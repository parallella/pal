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
    float r,r2,r4,r6,r8;
    float pi4 = 0.78539816339f;                      // constant=pi/4

    for ( i = 0; i < n; i++)
      {
       const float *pa = (a+i);
             float *pc = (c+i);
       ff = *pa;
       if (ff >= 0) sign = 1.0; else sign = -1.0;     // store sign of input variable
       if (ff < 0) x = ff * (-1.0f); else x = ff;     // this is equivalent of x=abs(a)

       r  = (x-1)/(x+1);
       r2 = r * r ; r4 = r2*r2 ; r6 = r4*r2 ; r8 = r4*r4 ;

       float z = pi4
        +    (+0.9999993329f*r
        +r2 *(-0.3332985605f*r
        +r  *(+0.1994653599f*r2
        +    (-0.1390853351f*r4
        +    (+0.0964200441f*r6
        +    (-0.0559098861f*r8
        +r4 *(+0.0218612288f*r6
        +    (-0.0040540580f*r8))))))));

       *pc = sign*z;
      }
}
