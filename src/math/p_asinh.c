#include <pal.h>
#include <math.h>

/**
 *
 * calculates the inverse hyperbolic sine of 'a'. The function does not check
 * for illegal input values.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 * @todo        Implement without using libm
 *
 */
void PSYM(p_asinh)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = asinhf(*(a + i));
    }
}
