#include <pal.h>
#include <stdint.h>
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
void p_popcount_u32(uint32_t *a, uint32_t *c, int n, int p, p_team_t team)
{
    static const unsigned int A[] = {0x55555555, 0x33333333,
                                     0x0f0f0f0f, 0x01010101};
    unsigned int *pa, *pc;
    unsigned int tmp;
    int i;

    pa = a;
    pc = c;

    for (i = 0; i < n; i++) {
        tmp = *pa - ((*pa >> 1) & A[0]);
        tmp = (tmp & A[1]) + ((tmp >> 2) & A[1]);
        *pc = ((tmp + (tmp >> 4)) & A[2]) * A[3] >> 24;
        pc++;
        pa++;
    }
}

void p_popcount_u64(uint64_t *a, uint64_t *c, int n, int p, p_team_t team)
{
    /* Implement me */
}

