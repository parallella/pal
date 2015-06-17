#include <pal.h>

/**
 *
 * Element wise vector addition between input vectors 'a' and 'b'
 *
 * @param a     Pointer to first input vector
 *
 * @param b     Pointer to second input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

#define GEN_FUNC_ADD(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
	{ \
		*(c) = *(a) + *(b);  \
		for (;--n;) \
			*(c + n) = *(a + n) + *(b + n); \
	}

GEN_FUNC_ADD(p_add_f32,float);

GEN_FUNC_ADD(p_add_int8,int8_t);
GEN_FUNC_ADD(p_add_uint8,uint8_t);

GEN_FUNC_ADD(p_add_int16,int16_t);
GEN_FUNC_ADD(p_add_uint16,uint16_t);

GEN_FUNC_ADD(p_add_int32,int32_t);
GEN_FUNC_ADD(p_add_uint32,uint32_t);

GEN_FUNC_ADD(p_add_int64,int64_t);
GEN_FUNC_ADD(p_add_uint64,uint64_t);

/**
 *
 * Element wise vector addition between input vector 'a' and scalar 'b'
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input scalar
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

#define GEN_FUNC_ADDS(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
	{ \
		*(c) = *(a) + *(b);  \
		for (;--n;) \
			*(c + n) = *(a + n) + *(b); \
	}

GEN_FUNC_ADDS(p_adds_f32,float);

GEN_FUNC_ADDS(p_adds_int8,int8_t);
GEN_FUNC_ADDS(p_adds_uint8,uint8_t);

GEN_FUNC_ADDS(p_adds_int16,int16_t);
GEN_FUNC_ADDS(p_adds_uint16,uint16_t);

GEN_FUNC_ADDS(p_adds_int32,int32_t);
GEN_FUNC_ADDS(p_adds_uint32,uint32_t);

GEN_FUNC_ADDS(p_adds_int64,int64_t);
GEN_FUNC_ADDS(p_adds_uint64,uint64_t);