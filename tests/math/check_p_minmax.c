/* p_minmax has a different signature from all other math functions in the
   PAL. It has 1 input vector and 2 output scalars and 2 output indices. As a
   result, it needs a distinct test infrastructure. */

#include <utest.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>

#include <pal.h>
/* TODO: This relative path include is fragile */
#include "../src/base/pal_base_private.h"
#include <common.h>

#include "check_p_minmax.h"

#ifndef FUNCTION
#error FUNCTION must be defined
#endif

#define GOLD_PATH XSTRING(gold/FUNCTION.gold.h)
#include GOLD_PATH

PTYPE *ai, *result1, *result2;
int *resultIndex1, *resultIndex2;

struct gold *gold = builtin_gold;
size_t gold_size = ARRAY_SIZE(builtin_gold);

/* For detecting erroneous overwrites */
#define OUTPUT_END_MARKER ((PTYPE)60189537703610376.0)

bool equals(PTYPE x, PTYPE y)
{
    PTYPE err;

    if (fabs(x - y) <= EPSILON_MAX)
        return true;

    if (fabs(x) > fabs(y))
        err = fabs((x - y) / x);
    else
        err = fabs((x - y) / y);

    return err <= EPSILON_RELMAX;
}

int setup(struct ut_suite *suite)
{
    size_t i;

    (void) suite;

    ai = calloc(gold_size, sizeof(PTYPE));

    /* Allocate one extra element for res and add end marker so overwrites can
     * be detected */
    result1 = calloc(2, sizeof(PTYPE));
    result1[1] = OUTPUT_END_MARKER;
    resultIndex1 = calloc(1, sizeof(int));

    result2 = calloc(2, sizeof(PTYPE));
    result2[1] = OUTPUT_END_MARKER;
    resultIndex2 = calloc(1, sizeof(int));

    for (i = 0; i < gold_size; i++) {
        ai[i] = gold[i].ai;
    }

    return 0;
}

int teardown(struct ut_suite *suite)
{
    free(ai);
    free(result1);
    free(result2);
    free(resultIndex1);
    free(resultIndex2);

    return 0;
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
    FUNCTION(ai, result1, result2, resultIndex1, resultIndex2, gold_size);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    ut_assert_msg(equals(result1[0], gold[0].gold1),
                  "%s: result 1: %f != %f",
                  XSTRING(FUNCTION), result1[0], gold[0].gold1);

    ut_assert_msg(equals(result2[0], gold[0].gold2),
                  "%s: result 2: %f != %f",
                  XSTRING(FUNCTION), result2[0], gold[0].gold2);

    ut_assert_msg(result1[1] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");

    ut_assert_msg(result2[1] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");

    // Skip checking the index

    return 0;
}

int tc_against_ref_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    if (gold_size == 0)
        return 0;

    PTYPE reference1, reference2;
    int indexOfReference1, indexOfReference2;
    generate_ref(&reference1, &reference2,
                 &indexOfReference1, &indexOfReference2, gold_size);

    ut_assert_msg(equals(reference1, gold[0].gold1),
                  "%s: result 1: %f != %f",
                  XSTRING(FUNCTION), result1[0], reference1);

    ut_assert_msg(equals(reference2, gold[0].gold2),
                  "%s: result 2: %f != %f",
                  XSTRING(FUNCTION), result2[0], reference2);

    ut_assert_msg(resultIndex1[0] == indexOfReference1,
                  "%s: index 1: %d != %d",
                  XSTRING(FUNCTION), resultIndex1[0], indexOfReference1);

    ut_assert_msg(resultIndex2[0] == indexOfReference2,
                  "%s: index 2: %d != %d",
                  XSTRING(FUNCTION), resultIndex2[0], indexOfReference2);

    return 0;
}

DECLARE_UT_TCASE(tc_against_gold, tc_against_gold_e, tc_against_gold_v, NULL);
DECLARE_UT_TCASE(tc_against_ref, NULL, tc_against_ref_v, NULL);

DECLARE_UT_TCASE_LIST(tcases, &tc_against_gold, &tc_against_ref);

#define FUNCTION_SUITE XCONCAT2(FUNCTION,_suite)
DECLARE_UT_SUITE(FUNCTION_SUITE, setup, teardown, false, tcases, NULL);

int main(int argc, char *argv[])
{
    int ret;
    char buf[1024] = { 0 };

    struct ut_suite *suite;

    suite = &FUNCTION_SUITE;

    ret = ut_run(suite);

    ut_report(buf, ARRAY_SIZE(buf), suite, true);

    printf("%s", buf);

    return ret;
}
