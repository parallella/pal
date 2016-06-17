#include <pal.h>
#include <stdint.h>

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

void PSYM(p_add)(const PTYPE *a, const PTYPE *b, PTYPE *c, int n)
{
#if defined(__epiphany__) && (P_FLOAT_TYPE == P_FLOAT_SINGLE)
    for (; n & 7; n--)
        *(c++) = *(a++) + *(b++);

    if (!n)
        return;

    if (!(((uintptr_t) a | (uintptr_t) b | (uintptr_t) c) & 7)) {
//#define HW_LOOPS
                n >>= 3;
        asm volatile(
# ifdef HW_LOOPS
                "gid\n"
                "mov   r16, %%low(.Lstart)\n"
                "movt  r16, %%high(.Lstart)\n"
                "movts ls,  r16\n"
                "mov   r16, %%low(.Lend)\n"
                "movt  r16, %%high(.Lend)\n"
                "movts le,  r16\n"
                "movts lc,  %[n]\n"
            ".balignw 8,0x01a2\n"               // align to 8-byte boundary
# endif
            ".Lstart:\n"
                "ldrd.l r16, [%[a]], #1\n"      // load  A 0-1
                "ldrd.l r18, [%[b]], #1\n"      // load  B 0-1

                "fadd.l r20, r16,    r18\n"     // add   0
                "ldrd.l r22, [%[a]], #1\n"      // load  A 2-3

                "fadd.l r21, r17,    r19\n"     // add   1
                "ldrd.l r24, [%[b]], #1\n"      // load  B 2-3

                "fadd.l r26, r22,    r24\n"     // add   2
                "strd.l r20, [%[c]], #1\n"      // store C 0-1

                "fadd.l r27, r23,    r25\n"     // add   3
                "ldrd.l r16, [%[a]], #1\n"      // load  A 4-5

                "ldrd.l r18, [%[b]], #1\n"      // load  B 4-5
                "fadd.l r20, r16,    r18\n"     // add   4

                "strd.l r26, [%[c]], #1\n"      // store C 2-3
                "fadd.l r21, r17,    r19\n"     // add   5

                "ldrd.l r22, [%[a]], #1\n"      // load  A 6-7
                "ldrd.l r24, [%[b]], #1\n"      // load  B 6-7

                "fadd.l r26, r22,    r24\n"     // add   6
                "strd.l r20, [%[c]], #1\n"      // store C 4-5
# ifdef HW_LOOPS
            ".balignw 8,0x01a2\n"               // align to 8-byte boundary
# endif
                "fadd.l r27, r23,    r25\n"     // add   7
# ifndef HW_LOOPS
                "sub %[n], %[n], #1\n"
# endif
            ".Lend:\n"
                "strd.l r26, [%[c]], #1\n"      // store C 6-7
# ifndef HW_LOOPS
                "bne .Lstart\n"
# endif
# ifdef HW_LOOPS
                "gie\n"
# endif
                : [a] "=r" (a), [b] "=r" (b), [c] "=r" (c), [n] "=r" (n)
                : "[a]" (a), "[b]" (b), "[c]" (c), "[n]" (n)
                : "memory", "cc",
                  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24",
                  "r25", "r26", "r27");
        return;
    }
#endif // __epiphany__
    for (int i = 0; i < n; i++)
        *(c + i) = *(a + i) + *(b + i);
}
