#include <pal.h>

#define ZERO_POINT_FIVE 0x3F00000000000000ULL // IEEE-754 64 bit float value for 0.5

/**
 *
 * Converts the double precision floating point values in 'a' to signed integer
 * values. Truncates towards zero to the nearest integer.
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

void p_dtoi(const double *a, int *c, int n)
{
    union {
        double d;
        uint64_t u;
    } rounding;

    for (int i = 0; i < n; i++) {
        rounding.d = *(a + i);
        rounding.u = (rounding.u & 0x8000000000000000ULL) | ZERO_POINT_FIVE;
        *(c+i) = (int) (*(a+i) + rounding.d);
    }
}
