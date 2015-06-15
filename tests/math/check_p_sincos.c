/* p_sincos has a different signature from all other math functions in the
   PAL. It has 1 input vector and 2 output vectors. As a result, it needs
   a distinct test infrastructure. */

#include <check.h>
#include "simple.h"
#include "gold/p_sincos_f32.gold.h"

float *ai, *resCos, *resSin, *refCos, *refSin;

struct gold *gold = builtin_gold;
size_t gold_size = ARRAY_SIZE(builtin_gold);

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

    ai = calloc(gold_size, sizeof(float));
    refCos = calloc(gold_size, sizeof(float));
    refSin = calloc(gold_size, sizeof(float));

    /* Allocate one extra element for res and add end marker so overwrites can
     * be detected */
    resCos = calloc(gold_size + 1, sizeof(float));
    resCos[gold_size] = OUTPUT_END_MARKER;
    resSin = calloc(gold_size + 1, sizeof(float));
    resSin[gold_size] = OUTPUT_END_MARKER;

    for (i = 0; i < gold_size; i++) {
        ai[i] = gold[i].ai;
    }

    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
    p_sincos_f32(ai, resSin, resCos, gold_size);
}

void teardown()
{
    free(ai);
    free(resCos);
    free(resSin);
    free(refCos);
    free(refSin);
}

START_TEST(GOLD_TEST)
{
    size_t i;

    for (i = 0; i < gold_size; i++) {
        ck_assert_msg(compare(resSin[i], gold[i].bi),
                      "p_sincos(%f): sin: %f != %f",
                      ai[i], resSin[i], gold[i].bi);
        ck_assert_msg(compare(resCos[i], gold[i].gold),
                      "p_sincos(%f): cos: %f != %f",
                      ai[i], resCos[i], gold[i].gold);
    }
    ck_assert_msg(resSin[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
    ck_assert_msg(resCos[i] == OUTPUT_END_MARKER,
                  "Output end marker was overwritten");
}
END_TEST

START_TEST(against_ref_function)
{
    size_t i;

    for (i = 0; i < gold_size; i++) {
        ck_assert_msg(compare(resSin[i], sinf(gold[i].ai)),
                      "p_sincos_f32(%f): sin: %f != %f",
                      ai[i], resSin[i], sinf(gold[i].ai));
        ck_assert_msg(compare(resCos[i], cosf(gold[i].ai)),
                      "p_sincos_f32(%f): cos: %f != %f",
                      ai[i], resCos[i], cosf(gold[i].ai));
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
    Suite *suite = suite_create("sincos_suite");
    TCase *tcase = tcase_create("sincos_tcase");
    SRunner *sr = srunner_create(suite);

    tcase_add_unchecked_fixture(tcase, setup, teardown);

    tcase_add_test(tcase, GOLD_TEST);
    tcase_add_test(tcase, against_ref_function);
    add_more_tests(tcase);

    suite_add_tcase(suite, tcase);

    srunner_run_all(sr, CK_ENV);
    num_failures = srunner_ntests_failed(sr);

    srunner_free(sr);

    return num_failures ? 1 : 0;
}
