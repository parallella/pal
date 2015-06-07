#include <pal.h>

/**
 *
 * Converts the floating point values in 'a' to signed integer values.
 *
 * @param a     Pointer to input vector, assume that no element is too close to LONG_MIN or LONG_MAX
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

void p_ftoi(const float *a, int *c, int n, int p, p_team_t team)
{
  const float *pa;
  int *pc;
  
  pa = a;
  pc = c;
  
  for (int i = 0; i<n; i++) {
    *pc = (*pa>=0) ? (int) (*pa+0.5) : (int) (*pa-0.5);
    pa++;
    pc++;
  }
}
