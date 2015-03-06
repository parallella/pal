#include <pal.h>

/**
 *
 * Counts the number of bits set in 'a'.
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
void p_popcount_u32(unsigned int *a, unsigned int *c, int n, int p, p_team_t team)
{
    /* Implement me */
}

void p_popcount_u64(unsigned int *a, unsigned int *c, int n, int p, p_team_t team)
{
    /* Implement me */
}

