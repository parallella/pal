#include <pal.h>

/**
 *
 * Calculates the hyperbolic cosine of the vector 'a'. Angles are specified
 * in radians.
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
void p_cosh_f32(const float *a, float *c, int n)
{
	int i;
	for (i = 0; i < n; i++) {
		const float *pa = (a+i);
		float *pc = (c+i);
		float val = 0;
		float theta = M_NORMALIZE_RADIANS(*pa);
		float theta_sq = theta*theta;

		val = 1.0f + theta_sq*0.01111111f*val;
		val = 1.0f + theta_sq*0.01785714f*val;
		val = 1.0f + theta_sq*0.03333333f*val;
		val = 1.0f + theta_sq*0.08333333f*val;
		val = 1.0f + theta_sq*0.50000000f*val;

		*pc = val;
	}
}
