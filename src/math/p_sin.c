#include <pal_math.h>
#include <pal_base.h>
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
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

#define SIN_ITERATIONS 5

void p_sin_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    int i;    
    for (i = 0; i < n; i++) {
      const float *pa = (a+i);
      float *pc = (c+i);
      float val = 1.0f;
      int k;
      float theta = *pa;
      //float theta = M_NORMALIZE_RADIANS(*pa);

      //for(k=SIN_ITERATIONS; k>=0; --k)
        //val = 1 - theta * ((theta / (2*k+2))/(2*k+3))*val;

      val = 1.0f - theta * theta * 0.083333333f * 0.076923077f * val;
      val = 1.0f - theta * theta * 0.1f * 0.090909091f * val;
      val = 1.0f - theta * theta * 0.125f * 0.111111111f * val;
      val = 1.0f - theta * theta * 0.166666667f * 0.142857143f * val;
      val = 1.0f - theta * theta * 0.25f * 0.2f * val;
      val = 1.0f - theta * theta * 0.5f * 0.333333333f * val;

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
