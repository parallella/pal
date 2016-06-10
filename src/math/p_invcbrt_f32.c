#include <pal.h>

/**
 *
 * Calculates the inverse cube root of the input vector 'a'.
 *
 * This routine uses a fast approximation to an inverse cube root with a similar
 * derivation to what is known as the 'Quake fast square root'.
 * It's highly likely that empirically better magic numbers may be determined.
 * And the approximation of division by 3 using (683*(j >> 11)) may have a
 * better alternative.  Regardless, the initial approximation error isn't bad
 * and the three Newton iterations get a very accurate result. -JAR
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
void p_invcbrt_f32(const float *a, float *c, int n)
{
	int i;
	float k = 1.33333333f;
	for (i=0; i<n; i++) {
		int j = *(int*)(a + i);
		int s = 0x80000000 & j;
		j &= 0x7fffffff;
		float a3 = 0.33333333f*(*(float*)&j);
		int y = 0x54a2fa8e - 683*(j >> 11);
		float x = *(float*)&y;
		x *= k - a3*x*x*x;
		x *= k - a3*x*x*x;
		x *= k - a3*x*x*x;
		int r = (s | (*(int*)&x));
		*(c + i) = *(float*)&r;
	}
}
