#include <pal.h>

#define ZERO_POINT_FIVE 0x3F000000 // IEEE-754 32 bit float value for 0.5f 

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
  uint32_t tmp;
  float rounding_value;

  for(int i = 0; i<n; i++){
    tmp = ((*(uint32_t*)(a + i)) & 0x80000000) | ZERO_POINT_FIVE;
    rounding_value = *((float*)&tmp);
    *(c+i) = (int) (*(a+i) + rounding_value);
   }
}
