#include <pal.h>
#include <stdio.h>
/**
 *
 * Calculate exponent (e^a), where e is the base of the natural logarithm
 * (2.71828.)
 *
 * Based of the algorithm in this paper: http://www.schraudolph.org/pubs/Schraudolph99.pdf,
 * It calculates a approximation of e^a very efficiently, but at the cost of accuracy.
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
void p_exp_f32(const float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        union
        {
            float f;
            uint32_t i;
        } u;

        // = 2^23 / M_LN2 * (*(a+i)) + (127 * 2^23 - 100000)
        u.i = 12102203.16156 * ( *( a + i )) + (1065353216 - 100000);
        *(c + i) = u.f;
    }
}
