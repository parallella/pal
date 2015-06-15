#include "simple.h"

#define GOLD_PATH XSTRING(gold/FUNCTION.gold.h)
#include GOLD_PATH

#ifndef FUNCTION
#error FUNCTION must be defined
#endif

#if !(defined(IS_UNARY) || defined(IS_BINARY))
#error IS_UNARY or IS_BINARY must be defined
#endif

struct gold *gold = builtin_gold;
size_t gold_size = ARRAY_SIZE(builtin_gold);

float *ai, *bi, *res, *ref;

bool generate_gold_flag = false;

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

    ai = calloc(gold_size, sizeof(float));
    bi = calloc(gold_size, sizeof(float));
    ref = calloc(gold_size, sizeof(float));

    /* Allocate one extra element for res and add end marker so overwrites can
     * be detected */
#ifdef SCALAR_OUTPUT
    res = calloc(2, sizeof(float));
    res[1] = OUTPUT_END_MARKER;
#else
    res = calloc(gold_size + 1, sizeof(float));
    res[gold_size] = OUTPUT_END_MARKER;
#endif
    for (i = 0; i < gold_size; i++) {
        ai[i] = gold[i].ai;
        bi[i] = gold[i].bi;
    }

    /* Run FUNCTION against gold input here so results are available
     * for all test cases. */
#if IS_UNARY
    FUNCTION(ai, res, gold_size);
#else /* Binary */
    FUNCTION(ai, bi, res, gold_size);
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
    for (i = 0; i < gold_size; i++)
        fprintf(ofp, "%f,%f,%f,%f\n", ai[i], bi[i], 0.0f, res[i]);
    fclose(ofp);

    fprintf(stdout, "Gold data written to: %s\n", XSTRING(FUNCTION.res));
    fprintf(stdout, "You need to manually copy it to gold/%s\n",
            XSTRING(FUNCTION.dat));
}
END_TEST

START_TEST(GOLD_TEST)
{
    size_t i;

    for (i = 0; i < gold_size; i++) {
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

    generate_ref(ref, gold_size);

    for (i = 0; i < gold_size; i++) {
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

void parse_options_or_die(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        if (!strncmp(argv[i], "--gold", ARRAY_SIZE("--gold")) ||
            !strncmp(argv[i], "-g", ARRAY_SIZE("-g")))
            generate_gold_flag = true;
        else
            goto usage;
    }
    return;

usage:
    fprintf(stderr,
"Usage: %s [OPTIONS]\n"
"\n"
"OPTIONS\n"
"\n"
"\t-g, --gold\tInstead of running test, generate gold data\n", argv[0]);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    int num_failures;
    size_t i;
    Suite *suite = suite_create(XSTRING(FUNCTION) "_suite");
    TCase *tcase = tcase_create(XSTRING(FUNCTION) "_tcase");
    SRunner *sr = srunner_create(suite);

    parse_options_or_die(argc, argv);

    tcase_add_unchecked_fixture(tcase, setup, teardown);

    if (generate_gold_flag) {
        tcase_add_test(tcase, print_gold);
    } else {
        tcase_add_test(tcase, GOLD_TEST);
        tcase_add_test(tcase, against_ref_function);
        add_more_tests(tcase);
    }

    suite_add_tcase(suite, tcase);

    srunner_run_all(sr, CK_ENV);
    num_failures = srunner_ntests_failed(sr);

    srunner_free(sr);

    return num_failures ? 1 : 0;
}
