#include <pal.h>

/**
 *
 * Element wise vector 'bitwise or' between input vectors 'a' and 'b'
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

#define GEN_FUNC_OR(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) | *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) | *(b + n); \
    }

GEN_FUNC_OR(p_or_int8,int8_t);
GEN_FUNC_OR(p_or_uint8,uint8_t);

GEN_FUNC_OR(p_or_int16,int16_t);
GEN_FUNC_OR(p_or_uint16,uint16_t);

GEN_FUNC_OR(p_or_int32,int32_t);
GEN_FUNC_OR(p_or_uint32,uint32_t);

GEN_FUNC_OR(p_or_int64,int64_t);
GEN_FUNC_OR(p_or_uint64,uint64_t);

/**
 *
 * Element wise vector 'bitwise or' between input vector 'a' and scalar 'b'
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

#define GEN_FUNC_ORS(NAME,TYPE) \
    /** NAME TYPE */ \
    void NAME(const TYPE * restrict a, const TYPE * restrict b, TYPE * restrict c, int n) \
    { \
        *(c) = *(a) | *(b);  \
        for (;--n;) \
            *(c + n) = *(a + n) | *(b); \
    }

GEN_FUNC_ORS(p_ors_int8,int8_t);
GEN_FUNC_ORS(p_ors_uint8,uint8_t);

GEN_FUNC_ORS(p_ors_int16,int16_t);
GEN_FUNC_ORS(p_ors_uint16,uint16_t);

GEN_FUNC_ORS(p_ors_int32,int32_t);
GEN_FUNC_ORS(p_ors_uint32,uint32_t);

GEN_FUNC_ORS(p_ors_int64,int64_t);
GEN_FUNC_ORS(p_ors_uint64,uint64_t);