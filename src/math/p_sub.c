#include <pal.h>

/**
 *
 * Element wise vector subtraction: c=a-b
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 * 
 * @return      None
 *
 */


#define GEN_FUNC_SUB(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
	{ \
		*(c) = *(a) - *(b);  \
		for (;--n;) \
			*(c + n) = *(a + n) - *(b + n); \
	}

GEN_FUNC_SUB(p_sub_f32,float);

GEN_FUNC_SUB(p_sub_int8,int8_t);
GEN_FUNC_SUB(p_sub_uint8,uint8_t);

GEN_FUNC_SUB(p_sub_int16,int16_t);
GEN_FUNC_SUB(p_sub_uint16,uint16_t);

GEN_FUNC_SUB(p_sub_int32,int32_t);
GEN_FUNC_SUB(p_sub_uint32,uint32_t);

GEN_FUNC_SUB(p_sub_int64,int64_t);
GEN_FUNC_SUB(p_sub_uint64,uint64_t);

/**
 *
 * Element wise vector substraction between input vector 'a' and scalar 'b'
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

#define GEN_FUNC_SUBS(NAME,TYPE) \
	/** NAME TYPE */ \
	void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
	{ \
		*(c) = *(a) - *(b);  \
		for (;--n;) \
			*(c + n) = *(a + n) - *(b); \
	}

GEN_FUNC_SUBS(p_subs_f32,float);

GEN_FUNC_SUBS(p_subs_int8,int8_t);
GEN_FUNC_SUBS(p_subs_uint8,uint8_t);

GEN_FUNC_SUBS(p_subs_int16,int16_t);
GEN_FUNC_SUBS(p_subs_uint16,uint16_t);

GEN_FUNC_SUBS(p_subs_int32,int32_t);
GEN_FUNC_SUBS(p_subs_uint32,uint32_t);

GEN_FUNC_SUBS(p_subs_int64,int64_t);
GEN_FUNC_SUBS(p_subs_uint64,uint64_t);