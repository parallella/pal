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
 * @return      None
 *
 */
void p_popcount_u32(const uint32_t *a, uint32_t *c, int n)
{
    static const uint32_t A[] = {0x55555555, 0x33333333,
                                 0x0f0f0f0f, 0x01010101};
    const uint32_t *pa;
    uint32_t *pc;
    uint32_t tmp;
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

void p_popcount_u64(const uint64_t *a, uint64_t *c, int n)
{
    static const uint64_t A[] = {0x5555555555555555, 0x3333333333333333,
                                 0x0f0f0f0f0f0f0f0f, 0x0101010101010101};
    const uint64_t *pa;
    uint64_t *pc;
    uint64_t tmp;
    int i;

    pa = a;
    pc = c;

    for (i = 0; i < n; i++) {
        tmp = *pa - ((*pa >> 1) & A[0]);
        tmp = (tmp & A[1]) + ((tmp >> 2) & A[1]);
        *pc = ((tmp + (tmp >> 4)) & A[2]) * A[3] >> 56;
        pc++;
        pa++;
    }
}

