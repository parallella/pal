#include <pal.h>

/*
 * -1 <= x <= 1
 * atan x = a1 * x + a3 * x^3 + ... + a9 * x^9 + e(x)
 * |e(x)| <= 10^-5
 */
static inline PTYPE _p_atan(const PTYPE x)
{
    const PTYPE a1 = PCONST(0.9998660);
    const PTYPE a3 = PCONST(-0.3302995);
    const PTYPE a5 = PCONST(0.1801410);
    const PTYPE a7 = PCONST(-0.0851330);
    const PTYPE a9 = PCONST(0.0208351);
    PTYPE x2 = x * x;
    return x * (a1 + x2 * (a3 + x2 * (a5 + x2 * (a7 + x2 * a9))));
}

/**
 *
 * Calculates inverse tangent (arc tangent) of the input value. The function
 * returns a value between -pi/2 to pi/2 but does not check for illegal input
 * values.
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
void PSYM(p_atan)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_atan(a[i]);
    }
}
