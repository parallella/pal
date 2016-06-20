#include <pal.h>

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define ISQRT_APPROX 0x5f375a86
#else
# define ISQRT_APPROX 0x5fe6eb50c7b537a9ULL
#endif

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
void PSYM(p_invsqrt)(const PTYPE *a, PTYPE *c, int n)
{
    // This union allows us to type-pun between integers and PTYPEs
    // with fewer strict aliasing concerns than the pointer casts
    // used in the original source.
    union
    {
        PUTYPE i;
        PTYPE f;
    } u;

    int i;
    for (i = 0; i < n; i++) {
        PTYPE x = a[i];
        const PTYPE x2 = x * PCONST(0.5);

        // Use some bit hacks to get a decent first approximation
        u.f = x;
        u.i = ISQRT_APPROX - (u.i >> 1);
        x = u.f;

        // Perform a couple steps of Newton's method to refine our guess
        x = x * (PCONST(1.5) - (x2 * x * x));
        x = x * (PCONST(1.5) - (x2 * x * x));
        x = x * (PCONST(1.5) - (x2 * x * x));
        c[i] = x;
    }
}
