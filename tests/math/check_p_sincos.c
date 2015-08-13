/* p_sincos has a different signature from all other math functions in the
   PAL. It has 1 input vector and 2 output vectors. As a result, it needs
   a distinct test infrastructure. */

#include <utest.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

#include <pal.h>
/* TODO: This relative path include is fragile */
#include "../../src/base/pal_base_private.h"
#include <common.h>

#include "simple.h"
#include "gold/p_sincos_f32.gold.h"

float *ai, *resCos, *resSin, *refCos, *refSin;

struct gold *gold = builtin_gold;
size_t gold_size = ARRAY_SIZE(builtin_gold);

/* For detecting erroneous overwrites */
#define OUTPUT_END_MARKER 60189537703610376.0f

bool compare(float x, float y)
{
    float err;

    if (fabs(x - y) <= EPSILON_MAX)
        return true;

    if (fabs(x) > fabs(y))
        err = fabs((x - y) / x);
    else
        err = fabs((x - y) / y);

    return err <= EPSILON_RELMAX;
}

#ifdef __epiphany__
void *simple_calloc(size_t nmemb, size_t size)
{
    void *p;
    uint64_t *q;
    size_t i;

    /* Find program break */
    p = sbrk(0);
    if (!p)
        return NULL;

    /* Align by double-word */
    if (!sbrk((8 - ((uintptr_t) p & 7)) & 7))
        return NULL;

    /* Calculate total size (assume no overflow) */
    size *= nmemb;

    /* Allocate in even double-words */
    size = (size + 7) & ~7;

    p = sbrk(size);
    if (!p)
        return NULL;

    /* Set to zero */
    for (i = 0, q = p; i < size; i += 8, q++)
        *q = 0;

    return p;
}
#define simple_free(p)
#else
#define simple_calloc calloc
#define simple_free free
#endif

int setup(struct ut_suite *suite)
{
    size_t i;

    (void) suite;

    ai = simple_calloc(gold_size, sizeof(float));
    refCos = simple_calloc(gold_size, sizeof(float));
    refSin = simple_calloc(gold_size, sizeof(float));

    /* Allocate one extra element for res and add end marker so overwrites can
     * be detected */
    resCos = simple_calloc(gold_size + 1, sizeof(float));
    resCos[gold_size] = OUTPUT_END_MARKER;
    resSin = simple_calloc(gold_size + 1, sizeof(float));
    resSin[gold_size] = OUTPUT_END_MARKER;

    for (i = 0; i < gold_size; i++) {
        ai[i] = gold[i].ai;
    }

    return 0;
}

int teardown(struct ut_suite *suite)
{
    (void) suite;

    simple_free(ai);
    simple_free(resCos);
    simple_free(resSin);
    simple_free(refCos);
    simple_free(refSin);

    return 0;
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
    p_sincos_f32(ai, resSin, resCos, gold_size);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;

    for (i = 0; i < gold_size; i++) {
        ut_assert_msg(compare(resSin[i], gold[i].bi),
                      "p_sincos(%f): sin: %f != %f",
                      ai[i], resSin[i], gold[i].bi);
        ut_assert_msg(compare(resCos[i], gold[i].gold),
                      "p_sincos(%f): cos: %f != %f",
                      ai[i], resCos[i], gold[i].gold);
    }
    ut_assert_msg(resSin[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
    ut_assert_msg(resCos[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");

    return 0;
}

int tc_against_ref_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;

    for (i = 0; i < gold_size; i++) {
        ut_assert_msg(compare(resSin[i], sinf(gold[i].ai)),
                      "p_sincos_f32(%f): sin: %f != %f",
                      ai[i], resSin[i], sinf(gold[i].ai));
        ut_assert_msg(compare(resCos[i], cosf(gold[i].ai)),
                      "p_sincos_f32(%f): cos: %f != %f",
                      ai[i], resCos[i], cosf(gold[i].ai));
    }

    return 0;
}

DECLARE_UT_TCASE(tc_against_gold, tc_against_gold_e, tc_against_gold_v, NULL);
DECLARE_UT_TCASE(tc_against_ref, NULL, tc_against_ref_v, NULL);

DECLARE_UT_TCASE_LIST(tcases, &tc_against_gold, &tc_against_ref);

#define FUNCTION_SUITE XCONCAT2(FUNCTION,_suite)
DECLARE_UT_SUITE(FUNCTION_SUITE, setup, teardown, false, tcases, NULL);

#ifdef __epiphany__
struct status {
    uint32_t done;
    uint32_t _pad1;
    uint32_t returncode;
    uint32_t _pad2;
} __attribute__((packed));

volatile struct status *epiphany_status = (struct status *) 0x8f200000;
volatile char *epiphany_results = (char *) 0x8f300000;
#endif

int main(int argc, char *argv[])
{
    int ret;
    char buf[1024] = { 0 };

    struct ut_suite *suite;

    suite = &FUNCTION_SUITE;

    ret = ut_run(suite);

    ut_report(buf, ARRAY_SIZE(buf), suite, true);
#ifdef __epiphany__
    memcpy((void *) epiphany_results, buf, sizeof(buf));
    epiphany_status->returncode = ret;
    epiphany_status->done = 1;
#else
    printf("%s", buf);
#endif

    return ret;
}
