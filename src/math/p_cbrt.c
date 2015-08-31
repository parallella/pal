#include <pal.h>

/**
 *
 * Calculates the cube root of the input vector 'a'.
 *
 * This is similar to the inverse cube root routine, p_invcbrt, with
 * two extra multiplication operations in the last line of code. -JAR
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
void p_cbrt_f32(const float *a, float *c, int n)
{
	int i;
	float k = 1.33333333f;
	for (i=0; i<n; i++) {
		float a0 = *(a+i);
		int j = *(int*)&a0;
		int s = 0x80000000 & j;
		j &= 0x7fffffff;
		float a3 = 0.33333333f*(*(float*)&j);
		int y = 0x54a2fa8e - 683*(j >> 11);
		float x = *(float*)&y;
		x *= k - a3*x*x*x;
		x *= k - a3*x*x*x;
		x *= k - a3*x*x*x;
		int r = (s|(*(int*)&x));
		float f = (*(float*)&r);
		*(c+i) = a0*f*f;
	}
}
