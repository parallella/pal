#include <pal.h>

/**
 *
 * Calculates the sumsq of the square of all of the elements vector 'a'.
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

#define GEN_FUNC(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE *a, TYPE * restrict c, int n) \
	{ \
    	*c = *(a) * *(a);  \
        for (;--n;) \
    	    *c += *(a + n) * *(a + n); \
    }

GEN_FUNC(p_sumsq_f32,float);

GEN_FUNC(p_sumsq_int8,int8_t);
GEN_FUNC(p_sumsq_uint8,uint8_t);

GEN_FUNC(p_sumsq_int16,int16_t);
GEN_FUNC(p_sumsq_uint16,uint16_t);

GEN_FUNC(p_sumsq_int32,int32_t);
GEN_FUNC(p_sumsq_uint32,uint32_t);

GEN_FUNC(p_sumsq_int64,int64_t);
GEN_FUNC(p_sumsq_uint64,uint64_t);