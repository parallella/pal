#include <pal.h>

/**
 *
 * Element wise division c = a / b.
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


#define GEN_FUNC_DIV(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) / *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) / *(b + n); \
    }

GEN_FUNC_DIV(p_div_f32,float);

GEN_FUNC_DIV(p_div_int8,int8_t);
GEN_FUNC_DIV(p_div_uint8,uint8_t);

GEN_FUNC_DIV(p_div_int16,int16_t);
GEN_FUNC_DIV(p_div_uint16,uint16_t);

GEN_FUNC_DIV(p_div_int32,int32_t);
GEN_FUNC_DIV(p_div_uint32,uint32_t);

GEN_FUNC_DIV(p_div_int64,int64_t);
GEN_FUNC_DIV(p_div_uint64,uint64_t);

/**
 *
 * Element wise vector division between input vector 'a' and scalar 'b'
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

#define GEN_FUNC_DIVS(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
    	float t = 1.0f / *(b); \
        *(c) = *(a) * t;  \
        for (;--n;) \
        *(c + n) = *(a + n) * t; \
    }

GEN_FUNC_DIVS(p_divs_f32,float);

GEN_FUNC_DIVS(p_divs_int8,int8_t);
GEN_FUNC_DIVS(p_divs_uint8,uint8_t);

GEN_FUNC_DIVS(p_divs_int16,int16_t);
GEN_FUNC_DIVS(p_divs_uint16,uint16_t);

GEN_FUNC_DIVS(p_divs_int32,int32_t);
GEN_FUNC_DIVS(p_divs_uint32,uint32_t);

GEN_FUNC_DIVS(p_divs_int64,int64_t);
GEN_FUNC_DIVS(p_divs_uint64,uint64_t);