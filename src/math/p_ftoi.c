#include <pal.h>

#define ZERO_POINT_FIVE 0x3F000000 // IEEE-754 32 bit float value for 0.5f 

/**
 *
 * Converts the floating point values in 'a' to signed integer values. Truncates towards zero to the nearest integer.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector
 *
 * @return      None
 *
 */

void p_ftoi(const float *a, int *c, int n)
{
    union {
        float f;
        uint32_t u;
    } rounding;
    {
    int i=0;    // outside of for loop for C compatibility

    for (; i < n; i++) {
        rounding.f = *(a + i);
        rounding.u = (rounding.u & 0x80000000) | ZERO_POINT_FIVE;
        *(c+i) = (int) (*(a+i) + rounding.f);
    }
    }
}
