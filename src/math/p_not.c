#include <pal.h>

/**
 *
 * Element wise vector 'bitwise not' of input vector 'a'
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

#define GEN_FUNC(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE * restrict a, TYPE * restrict c, int n) \
	{ \
    	*c = ~*(a);  \
        for (;--n;) \
    	    *c = ~*(a + n); \
    }


GEN_FUNC(p_not_int8,int8_t);
GEN_FUNC(p_not_uint8,uint8_t);

GEN_FUNC(p_not_int16,int16_t);
GEN_FUNC(p_not_uint16,uint16_t);

GEN_FUNC(p_not_int32,int32_t);
GEN_FUNC(p_not_uint32,uint32_t);

GEN_FUNC(p_not_int64,int64_t);
GEN_FUNC(p_not_uint64,uint64_t);