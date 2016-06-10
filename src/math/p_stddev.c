#include <pal.h>

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define ISQRT_APPROX 0x5f375a86
#else
# define ISQRT_APPROX 0x5fe6eb50c7b537a9ULL
#endif

/**
 *
 * Calculates the standard deviation of all of the elements vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @return      None
 *
 */

void PSYM(p_stddev)(const PTYPE *a, PTYPE *c, int n)
{
    PTYPE tmp = 0.0f, mean = 0.0f, meansq = 0.0f;
    int i;

    for (i = 0; i < n; i++) {
        tmp += *(a + i);
    }
    mean = tmp / n;

    tmp = 0.0f;
    for (i = 0; i < n; i++) {
        tmp += (*(a + i) - mean) * (*(a + i) - mean);
    }
    meansq = tmp / (n - 1);

    PTYPE x;
    union {
       PTYPE f;
       PITYPE i;
    } j;
    PTYPE xhalf = 0.5*meansq;

    j.f = meansq;
    j.i = ISQRT_APPROX - (j.i >> 1);
    x = j.f;

    // Newton steps, repeating this increases accuracy
    x = x*(1.5 - xhalf*x*x);
    x = x*(1.5 - xhalf*x*x);
    x = x*(1.5 - xhalf*x*x);

    // x contains the inverse sqrt

    // Multiply the inverse sqrt by the input to get the sqrt
    *c = meansq * x;
}
