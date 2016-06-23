#include <pal.h>

/**
 *
 * Compute the sine of the vector 'a'. Angles are specified in radians.
 * The radian number must be in the range 0 to 2pi,
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

#define SIN_ITERATIONS 5

void PSYM(p_sin)(const PTYPE *a, PTYPE *c, int n)
{
    int i;    
    for (i = 0; i < n; i++) {
      const PTYPE *pa = (a+i);
      PTYPE *pc = (c+i);
      PTYPE val = PCONST(1.0);
      int k;
      PTYPE theta = *pa;
      PTYPE theta2 = *pa * *pa;
      //PTYPE theta = M_NORMALIZE_RADIANS(*pa);

      //for(k=SIN_ITERATIONS; k>=0; --k)
        //val = 1 - theta * ((theta / (2*k+2))/(2*k+3))*val;

      val = PCONST(1.0) - theta2 * PCONST(0.083333333) * PCONST(0.076923077)* val;
      val = PCONST(1.0) - theta2 * PCONST(0.1) * PCONST(0.090909091) * val;
      val = PCONST(1.0) - theta2 * PCONST(0.125) * PCONST(0.111111111) * val;
      val = PCONST(1.0) - theta2 * PCONST(0.166666667) * PCONST(0.142857143) * val;
      val = PCONST(1.0) - theta2 * PCONST(0.25) * PCONST(0.2) * val;
      val = PCONST(1.0) - theta2 * PCONST(0.5) * PCONST(0.333333333) * val;

      *pc = theta * val;
    }

#if 0
#ifdef TARGET_EPIPHANY
    //1. resolve location of memory
    //2. p-write: push out data to cores (p_rmalloc?)
    //3. set input arguments and output arguments (R0-R3) for each task
    //3. p_run for all cores
    //4. each core writes back to the C array
    //5. wait() for all p_runs to get done
    //6. done
#else
    //1.System type ("SMP") from p_team_t
    int type=3;

    //2. Get the ISA ("x86") from p_team_t
    int isa=2;
    
    //3. Get the O/S ("posix/linux") from p_team_t
    int os = 2;

    if(type==P_DEV_SMP){
    }
#endif
#endif
   
}
