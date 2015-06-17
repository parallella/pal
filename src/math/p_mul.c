#include <pal.h>

/**
 *
 * Element wise vector multiplication between input vectors 'a' and 'b'
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


#define GEN_FUNC_MUL(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) * *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) * *(b + n); \
    }

GEN_FUNC_MUL(p_mul_f32,float);

GEN_FUNC_MUL(p_mul_int8,int8_t);
GEN_FUNC_MUL(p_mul_uint8,uint8_t);

GEN_FUNC_MUL(p_mul_int16,int16_t);
GEN_FUNC_MUL(p_mul_uint16,uint16_t);

GEN_FUNC_MUL(p_mul_int32,int32_t);
GEN_FUNC_MUL(p_mul_uint32,uint32_t);

GEN_FUNC_MUL(p_mul_int64,int64_t);
GEN_FUNC_MUL(p_mul_uint64,uint64_t);

/**
 *
 * Element wise vector multiplication between input vector 'a' and scalar 'b'
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

#define GEN_FUNC_MULS(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) * *(b);  \
        for (;--n;) \
        *(c + n) = *(a + n) * *(b); \
    }

GEN_FUNC_MULS(p_muls_f32,float);

GEN_FUNC_MULS(p_muls_int8,int8_t);
GEN_FUNC_MULS(p_muls_uint8,uint8_t);

GEN_FUNC_MULS(p_muls_int16,int16_t);
GEN_FUNC_MULS(p_muls_uint16,uint16_t);

GEN_FUNC_MULS(p_muls_int32,int32_t);
GEN_FUNC_MULS(p_muls_uint32,uint32_t);

GEN_FUNC_MULS(p_muls_int64,int64_t);
GEN_FUNC_MULS(p_muls_uint64,uint64_t);