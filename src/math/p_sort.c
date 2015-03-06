#include <pal.h>

/**
 *
 * Sorts an array of values
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to result vector
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
void p_sort_f32(float *a, float *c, int n, int p, p_team_t team) {}

void p_sort_u32(int *a, int *c, int n, int p, p_team_t team) {}
