#ifndef FUNCTION
#error FUNCTION must be defined
#endif

#if !(defined(IS_UNARY) || defined(IS_BINARY))
#error IS_UNARY or IS_BINARY must be defined
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>

#include <check.h>
#include <pal.h>
#include "../../src/base/pal_base_private.h"
#include <common.h>

struct gold {
    float ai;
    float bi;
    float res;
    float gold;
};

#define GOLD_PATH XSTRING(gold/FUNCTION.gold.h)
#include GOLD_PATH

float *ai, *bi, *res;

#define EPSILON_MAX 0.001f
#define EPSILON_RELMAX 0.00001f
/* For detecting erroneous overwrites */
#define OUTPUT_END_MARKER 60189537703610376.0f
__attribute__((weak))
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

#ifdef GENERATE_GOLD
void print_gold()
{
    size_t i;
    FILE *ofp;

    ofp = fopen(XSTRING(FUNCTION.res), "w");
    for (i = 0; i < ARRAY_SIZE(gold); i++)
        fprintf(ofp, "%f,%f,%f,%f\n", ai[i], bi[i], 0.0f, res[i]);
    fclose(ofp);
}
#endif

#if IS_UNARY
START_TEST(CONCAT2(test_, FUNCTION))
{
    size_t i;

    /* HACK: Pass in an invalid team. API was changed in:
     * a380f6b70b8461dbb8c0def388d00270f8b27c28
     * but implementation did have not catched up yet.
     * When it does, the tests will break... */
    FUNCTION(ai, res, ARRAY_SIZE(gold), 0, p_ref_err(EINVAL));
#ifdef GENERATE_GOLD
    print_gold();
#else
    for (i = 0; i < ARRAY_SIZE(gold); i++) {
        ck_assert_msg(compare(res[i], gold[i].gold), "%s(%f): %f != %f",
                      XSTRING(FUNCTION), ai[i], res[i], gold[i].gold);
#ifdef SCALAR_OUTPUT /* Scalar output so only first address is valid */
        i++;
        break;
#endif
    }
    ck_assert_msg(res[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
#endif
}
END_TEST
#else
START_TEST(CONCAT2(test_, FUNCTION))
{
    size_t i;

    /* HACK: see above comment */
    FUNCTION(ai, bi, res, ARRAY_SIZE(gold), 0, p_ref_err(EINVAL));
#ifdef GENERATE_GOLD
    print_gold();
#else
    for (i = 0; i < ARRAY_SIZE(gold); i++) {
        ck_assert_msg(compare(res[i], gold[i].gold), "%s(%f, %f): %f != %f",
                      XSTRING(FUNCTION), ai[i], bi[i], res[i],
                      gold[i].gold);
#ifdef SCALAR_OUTPUT /* Scalar output so only first address is valid */
        i++;
        break;
#endif
    }
    ck_assert_msg(res[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
#endif
}
END_TEST
#endif /* IS_UNARY */

__attribute__((weak))
int main(void)
{
    int num_failures;
    size_t i;
    Suite *suite = suite_create(XSTRING(FUNCTION) "_suite");
    TCase *tcase = tcase_create(XSTRING(FUNCTION) "_tcase");
    SRunner *sr = srunner_create(suite);

    suite_add_tcase(suite, tcase);
    tcase_add_test(tcase, CONCAT2(test_, FUNCTION));

    ai = calloc(ARRAY_SIZE(gold), sizeof(float));
    bi = calloc(ARRAY_SIZE(gold), sizeof(float));

    /* Allocate one extra element for res and add end marker so overwrites can
     * be detected */
#ifdef SCALAR_OUTPUT
    res = calloc(2, sizeof(float));
    res[1] = OUTPUT_END_MARKER;
#else
    res = calloc(ARRAY_SIZE(gold) + 1, sizeof(float));
    res[ARRAY_SIZE(gold)] = OUTPUT_END_MARKER;
#endif
    for (i = 0; i < ARRAY_SIZE(gold); i++) {
        ai[i] = gold[i].ai;
        bi[i] = gold[i].bi;
    }

    srunner_run_all(sr, CK_ENV);
    num_failures = srunner_ntests_failed(sr);

    free(ai);
    free(bi);
    free(res);

    srunner_free(sr);

    return num_failures ? 1 : 0;
}
