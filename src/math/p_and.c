#include <pal.h>

/**
 *
 * Element wise vector 'bitwise and' between input vectors 'a' and 'b'
 *
 * @param a     Pointer to first input vector
 *
 * @param b     Pointer to second input vector
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

void p_and(const uint32_t *a, const uint32_t *b, uint32_t *c,
               int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = *(a + i) & *(b + i);
    }
}

/**
 *
 * Element wise vector 'bitwise and' between input vector 'a' and scalar 'b'
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input scalar
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

void p_and_s(const uint32_t *a, const uint32_t *b, uint32_t *c,
               int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = *(a + i) & *(b);
    }
}
