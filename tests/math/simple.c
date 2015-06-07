#include "simple.h"

#define GOLD_PATH XSTRING(gold/FUNCTION.gold.h)
#include GOLD_PATH

#ifndef FUNCTION
#error FUNCTION must be defined
#endif

#if !(defined(IS_UNARY) || defined(IS_BINARY))
#error IS_UNARY or IS_BINARY must be defined
#endif

float *ai, *bi, *res, *ref;

#define GOLD_TEST XCONCAT2(test_gold_, FUNCTION)

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

void setup()
{
    size_t i;

    ai = calloc(ARRAY_SIZE(gold), sizeof(float));
    bi = calloc(ARRAY_SIZE(gold), sizeof(float));
    ref = calloc(ARRAY_SIZE(gold), sizeof(float));

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

    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
    /* HACK: Pass in an invalid team. API was changed in:
     * a380f6b70b8461dbb8c0def388d00270f8b27c28
     * but implementation did have not catched up yet.
     * When it does, the tests will break... */
#if IS_UNARY
    FUNCTION(ai, res, ARRAY_SIZE(gold), 0, p_ref_err(EINVAL));
#else /* Binary */
    FUNCTION(ai, bi, res, ARRAY_SIZE(gold), 0, p_ref_err(EINVAL));
#endif
}

void teardown()
{
    free(ai);
    free(bi);
    free(res);
    free(ref);
}

START_TEST(print_gold)
{
    size_t i;
    FILE *ofp;

    ofp = fopen(XSTRING(FUNCTION.res), "w");
    for (i = 0; i < ARRAY_SIZE(gold); i++)
        fprintf(ofp, "%f,%f,%f,%f\n", ai[i], bi[i], 0.0f, res[i]);
    fclose(ofp);

    fprintf(stdout, "Gold data written to: %s\n", XSTRING(FUNCTION.res));
}
END_TEST

START_TEST(GOLD_TEST)
{
    size_t i;

    for (i = 0; i < ARRAY_SIZE(gold); i++) {
#if IS_UNARY
        ck_assert_msg(compare(res[i], gold[i].gold), "%s(%f): %f != %f",
                      XSTRING(FUNCTION), ai[i], res[i], gold[i].gold);
#else
        ck_assert_msg(compare(res[i], gold[i].gold), "%s(%f, %f): %f != %f",
                      XSTRING(FUNCTION), ai[i], bi[i], res[i], gold[i].gold);
#endif
#ifdef SCALAR_OUTPUT /* Scalar output so only first address is valid */
        i++;
        break;
#endif
    }
    ck_assert_msg(res[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
}
END_TEST

/* Default to using gold data as reference function output */
__attribute__((weak))
void generate_ref(float *out, size_t n)
{
    size_t i;

    for (i = 0; i < n; i++)
        out[i] = gold[i].gold;
}

START_TEST(against_ref_function)
{
    size_t i;

    generate_ref(ref, ARRAY_SIZE(gold));

    for (i = 0; i < ARRAY_SIZE(gold); i++) {
#if IS_UNARY
        ck_assert_msg(compare(res[i], ref[i]), "%s(%f): %f != %f",
                      XSTRING(FUNCTION), ai[i], res[i], ref[i]);
#else
        ck_assert_msg(compare(res[i], ref[i]), "%s(%f, %f): %f != %f",
                      XSTRING(FUNCTION), ai[i], bi[i], res[i], ref[i]);
#endif
#ifdef SCALAR_OUTPUT /* Scalar output so only first address is valid */
        i++;
        break;
#endif
    }
}
END_TEST

/* Allow individual tests to add more test cases, e.g. against a reference
 * function */
__attribute__((weak))
void add_more_tests(TCase *tcase)
{
}

int main(void)
{
    int num_failures;
    size_t i;
    Suite *suite = suite_create(XSTRING(FUNCTION) "_suite");
    TCase *tcase = tcase_create(XSTRING(FUNCTION) "_tcase");
    SRunner *sr = srunner_create(suite);

    tcase_add_unchecked_fixture(tcase, setup, teardown);

#ifdef GENERATE_GOLD
    tcase_add_test(tcase, print_gold);
#else
    tcase_add_test(tcase, GOLD_TEST);
    tcase_add_test(tcase, against_ref_function);
    add_more_tests(tcase);
#endif

    suite_add_tcase(suite, tcase);

    srunner_run_all(sr, CK_ENV);
    num_failures = srunner_ntests_failed(sr);

    srunner_free(sr);

    return num_failures ? 1 : 0;
}
