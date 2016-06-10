#include <pal.h>

/**
 *
 * Calculates the base 10 logarithm of 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void PSYM(p_log10)(const PTYPE *a, PTYPE *c, int n)
{
    int i;

    PSYM(p_ln)(a, c, n);
    for (i = 0; i < n; i++) {
        // log10(x) = ln(x) / ln(10)
        c[i] *= M_DIV_LN10;
    }
}
