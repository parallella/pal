#include <pal.h>

/**
 *
 * Calculates the cross product between vectors 'a' and 'b', producing
 * another vector result 'c'.
 *
 * @param a     Pointer to input 3d vector
 *
 * @param b     Pointer to input 3d vector
 *
 * @param c     Pointer to output 3d vector
 *
 * @param mag	Pointer to output magnitude scalar (set null to not compute)
 *
 * @return      None
 *
 */
void p_cross_f32(const float *a, const float *b, float *c, float *mag)
{
	float tmp;

    c[0] = a[1]*b[2] - a[2]*b[1];

    c[1] = a[2]*b[0] - a[0]*b[2];

    c[2] = a[0]*b[1] - a[1]*b[0];

    if(mag)
    {
	    tmp = c[0]*c[0] + c[1]*c[1] + c[2]*c[2];
	    p_sqrt_f32(&tmp, mag, 1);
	}
}
