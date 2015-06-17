#include <pal.h>

/**
 *
 * Element wise vector 'bitwise xor' between input vectors 'a' and 'b'
 *
 * @param a     Pointer to first input vector
 *
 * @param b     Pointer to second input vector
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

#define GEN_FUNC_XOR(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) ^ *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) ^ *(b + n); \
    }

GEN_FUNC_XOR(p_xor_int8,int8_t);
GEN_FUNC_XOR(p_xor_uint8,uint8_t);

GEN_FUNC_XOR(p_xor_int16,int16_t);
GEN_FUNC_XOR(p_xor_uint16,uint16_t);

GEN_FUNC_XOR(p_xor_int32,int32_t);
GEN_FUNC_XOR(p_xor_uint32,uint32_t);

GEN_FUNC_XOR(p_xor_int64,int64_t);
GEN_FUNC_XOR(p_xor_uint64,uint64_t);

/**
 *
 * Element wise vector 'bitwise xor' between input vector 'a' and scalar 'b'
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input scalar
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

#define GEN_FUNC_XORS(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) ^ *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) ^ *(b); \
    }

GEN_FUNC_XORS(p_xors_int8,int8_t);
GEN_FUNC_XORS(p_xors_uint8,uint8_t);

GEN_FUNC_XORS(p_xors_int16,int16_t);
GEN_FUNC_XORS(p_xors_uint16,uint16_t);

GEN_FUNC_XORS(p_xors_int32,int32_t);
GEN_FUNC_XORS(p_xors_uint32,uint32_t);

GEN_FUNC_XORS(p_xors_int64,int64_t);
GEN_FUNC_XORS(p_xors_uint64,uint64_t);