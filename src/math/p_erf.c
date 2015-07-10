#include <pal.h>
/**
*
* Calculate error function
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

void p_erf_f32(const float *a, float *c, int n)

{ 
    int i; int sign; float x;
    for ( i = 0; i < n; i++)
      { 
       const float *pa = (a+i); 
             float *pc = (c+i);

       // the computation is based on formula 7.1.28 from Abramowitz and Stegun for a >= 0

       if (*pa >= 0) sign = 1.0; else sign = -1.0;       // store sign of input variable
       if (*pa < 0) x = *pa * (-1.0f); else x = *pa;     // this is equivalent of x=abs(a)
       if (x >= 3.6f) *pc = 1.0f * sign;                 // c = 1 * sign if a >=3.6 else calculate error function
       else
	{
        // constants for formula 7.1.28 from Abramowitz and Stegun
        float a1 =  0.0705230784f;
        float a2 =  0.0422820123f;
        float a3 =  0.0092705272f;
        float a4 =  0.0001520143f;
        float a5 =  0.0002765672f;
        float a6 =  0.0000430638f;

        //formula 7.1.28 from Abramowitz and Stegun
        float t = 1.0f / ((((((a6 * x + a5) * x + a4) * x + a3) * x + a2) * x + a1) * x + 1.0f);
        float z = 1.0f - (t * t * t * t * t * t * t * t * t * t * t * t * t * t * t * t) ;
        *pc = z*sign;
        }
      }
}
