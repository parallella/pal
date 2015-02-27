#include <pal.h>

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
#include <math.h>
void p_atan_32f(float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = atan(*(a + i));
    }
}
