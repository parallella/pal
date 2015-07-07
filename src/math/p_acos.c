#include <math.h>
#include <pal.h>

static const float pi_2 = (float) M_PI / 2.f;

/**
 *
 * Computes the inverse cosine (arc cosine) of the input vector 'a'. Input
 * values to acos must be in the range -1 to 1. The result values are in the
 * range 0 to pi. The function does not check for illegal input values.
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
void p_acos_f32(const float *a, float *c, int n)
{

    int i;
    float tmp;
    /* acos x = pi/2 - asin x */
    p_asin_f32(a, c, n);
    for (i = 0; i < n; i++) {
        tmp = pi_2 - c[i];
        c[i] = tmp;
    }
}
