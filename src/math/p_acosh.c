#include <pal.h>
#include <math.h>

/**
 *
 * Calculates the inverse hyperbolic cosine of 'a'. Input values must be > 1.
 * The function does not check for illegal input values.
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
void PSYM(p_acosh)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = acoshf(*(a + i));
    }
}
