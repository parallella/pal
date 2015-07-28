#include <pal.h>

/**
 *
 * Element wise vector 'bitwise and' between input vectors 'a' and 'b'
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

#define GEN_FUNC_AND(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) & *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) & *(b + n); \
    }

GEN_FUNC_AND(p_and_int8,int8_t);
GEN_FUNC_AND(p_and_uint8,uint8_t);

GEN_FUNC_AND(p_and_int16,int16_t);
GEN_FUNC_AND(p_and_uint16,uint16_t);

GEN_FUNC_AND(p_and_int32,int32_t);
GEN_FUNC_AND(p_and_uint32,uint32_t);

GEN_FUNC_AND(p_and_int64,int64_t);
GEN_FUNC_AND(p_and_uint64,uint64_t);

/**
 *
 * Element wise vector 'bitwise and' between input vector 'a' and scalar 'b'
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

#define GEN_FUNC_ANDS(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) & *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) & *(b); \
    }

GEN_FUNC_ANDS(p_ands_int8,int8_t);
GEN_FUNC_ANDS(p_ands_uint8,uint8_t);

GEN_FUNC_ANDS(p_ands_int16,int16_t);
GEN_FUNC_ANDS(p_ands_uint16,uint16_t);

GEN_FUNC_ANDS(p_ands_int32,int32_t);
GEN_FUNC_ANDS(p_ands_uint32,uint32_t);

GEN_FUNC_ANDS(p_ands_int64,int64_t);
GEN_FUNC_ANDS(p_ands_uint64,uint64_t);