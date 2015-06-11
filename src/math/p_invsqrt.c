#include <pal.h>

/**
 *
 * Calculates the inverse square root of the input vector 'a'.
 *
 * This function uses a method of computing the inverse square root
 * made popular by the Quake 3 source code release. Chris Lomont has
 * provided an exhaustive analysis here: 
 * http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
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
void p_invsqrt_f32(const float *a, float *c, int n)
{
    // This union allows us to type-pun between integers and floats
    // with fewer strict aliasing concerns than the pointer casts
    // used in the original source.
    union
    {
        int32_t i;
        float f;
    } u;

    int i;
    for (i = 0; i < n; i++) {
        float x = a[i];
        float x2 = x * 0.5f;

        // Use some bit hacks to get a decent first approximation
        u.f = x;
        u.i = 0x5f375a86 - (u.i >> 1);
        x = u.f;

        // Perform a couple steps of Newton's method to refine our guess
        x = x * (1.5f - (x2 * x * x));
        x = x * (1.5f - (x2 * x * x));

        c[i] = x;
    }
}