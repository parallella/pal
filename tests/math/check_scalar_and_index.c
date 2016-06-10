/* This type of function has a different signature from most other scalar math
   functions in the PAL. It has 1 input vector and 1 output scalar and 1 output
   index.  As a result, it needs a distinct test infrastructure. */

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

#include "check_scalar_and_index.h"

#ifndef FUNCTION
#error FUNCTION must be defined
#endif

#define GOLD_PATH XSTRING(gold/FUNCTION.gold.h)
#include GOLD_PATH

PTYPE *ai, *result;
int *resultIndex;

struct gold *gold = builtin_gold;
size_t gold_size = ARRAY_SIZE(builtin_gold);

/* For detecting erroneous overwrites */
#define OUTPUT_END_MARKER 60189537703610376.0f

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
    result = calloc(2, sizeof(PTYPE));
    result[1] = OUTPUT_END_MARKER;
    resultIndex = calloc(1, sizeof(int));

    for (i = 0; i < gold_size; i++) {
        ai[i] = gold[i].ai;
    }

    return 0;
}

int teardown(struct ut_suite *suite)
{
    free(ai);
    free(result);
    free(resultIndex);

    return 0;
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
    FUNCTION(ai, result, resultIndex, gold_size);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;

    ut_assert_msg(equals(result[0], gold[0].gold),
                  "p_max: max: %f != %f",
                  result[0], gold[0].gold);

    ut_assert_msg(result[1] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");

    // Skip checking the index

    return 0;
}

int tc_against_ref_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    int i;
    if (gold_size == 0)
        return 0;

    PTYPE reference;
    int indexOfReference;
    generate_ref(&reference, &indexOfReference, gold_size);

    ut_assert_msg(equals(reference, gold[0].gold),
                  "%s: result: %f != %f",
                  XSTRING(FUNCTION), result[0], reference);

    ut_assert_msg(resultIndex[0] == indexOfReference,
                  "%s: index: %d != %d",
                  XSTRING(FUNCTION), resultIndex[0], indexOfReference);

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
