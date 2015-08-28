#include <utest.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <math.h>
#include <pal.h>
#include <common.h>

#define MAX_ABS_DIFF  0.0001
#define CANARY 0x5050aa55
#define OK 1
#define NOK 0

/* Returns true on success */
bool compare(float res, float ref)
{
    return (fabs(res - ref) <= MAX_ABS_DIFF);
}

#define TRIVIAL_COLS 3
#define TRIVIAL_ROWS 3

float trivial_in[TRIVIAL_COLS * TRIVIAL_ROWS] = {
9.0,        6.0,        4.0,
1.0,        2.0,        5.0,
8.0,        7.0,        3.0,
};

/* Borders not computed */
float trivial_expected[(TRIVIAL_COLS-2) * (TRIVIAL_ROWS-2)] = {
/*x*/        /*x*/       /*x*/
/*x*/        5.0,        /*x*/
/*x*/        /*x*/       /*x*/
};

float trivial_out[(TRIVIAL_COLS-2) * (TRIVIAL_ROWS-2) + 1];

int tc_trivial_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    *(int*)(&trivial_out[ARRAY_SIZE(trivial_out) - 1]) = CANARY;

    p_median3x3_f32(trivial_in, trivial_out, TRIVIAL_ROWS, TRIVIAL_COLS);

    return 0;
}

int tc_trivial_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < ARRAY_SIZE(trivial_expected); i++) {
        ut_assert_msg(compare(trivial_out[i], trivial_expected[i]),
                      "p_median3x3_f32(): Large diff for index: %d, ref: %f, test: %f, diff: %f\n",
                      i, trivial_expected[i], trivial_out[i], trivial_expected[i]-trivial_out[i]);
    }

    ut_assert_msg((*(int*)(&trivial_out[ARRAY_SIZE(trivial_out) - 1]) == CANARY),
                  "p_median3x3_f32(): CANARY overwritten\n");

    return 0;
}

#define test5x5_COLS 5
#define test5x5_ROWS 5

float test5x5_in[test5x5_COLS * test5x5_ROWS] = {
-0.107358,-0.049577,+0.126623,-0.106948,-0.755809,
+0.051781,-0.265425,+0.548136,+0.051735,-0.026964,
+0.941475,-0.975603,+0.411370,+0.755263,-0.055232,
-0.770979,-0.360797,-0.476552,-0.656793,-0.346903,
+0.738044,-0.380252,-0.769260,+0.635805,-0.476552,
};

/* Borders not computed */
float test5x5_expected[(test5x5_COLS-2) * (test5x5_ROWS-2)] = {
0.051781000000000001, 0.051735000000000003, 0.051735000000000003,
-0.26542500000000002, -0.26542500000000002, -0.026963999999999998,
-0.38025199999999998, -0.38025199999999998, -0.34690300000000002
};

float test5x5_out[(test5x5_COLS-2) * (test5x5_ROWS-2) + 1];

int tc_test5x5_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    *(int*)(&test5x5_out[ARRAY_SIZE(test5x5_out) - 1]) = CANARY;

    p_median3x3_f32(test5x5_in, test5x5_out, test5x5_ROWS, test5x5_COLS);

    return 0;
}

int tc_test5x5_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < ARRAY_SIZE(test5x5_expected); i++) {
        ut_assert_msg(compare(test5x5_out[i], test5x5_expected[i]),
                      "p_median3x3_f32(): Large diff for index: %d, ref: %f, test: %f, diff: %f\n",
                      i, test5x5_expected[i], test5x5_out[i], test5x5_expected[i]-test5x5_out[i]);
    }

    ut_assert_msg((*(int*)(&test5x5_out[ARRAY_SIZE(test5x5_out) - 1]) == CANARY),
                  "p_median3x3_f32(): CANARY overwritten\n");

    return 0;
}

int setup(struct ut_suite *suite)
{
    (void) suite;

    return 0;
}

int teardown(struct ut_suite *suite)
{
    (void) suite;

    return 0;
}

DECLARE_UT_TCASE(tc_trivial, tc_trivial_e, tc_trivial_v, NULL);
DECLARE_UT_TCASE(tc_test5x5, tc_test5x5_e, tc_test5x5_v, NULL);
DECLARE_UT_TCASE_LIST(tcases, &tc_trivial, &tc_test5x5);
DECLARE_UT_SUITE(p_median3x3_f32_suite, setup, teardown, false, tcases, NULL);

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

    struct ut_suite *suite = &p_median3x3_f32_suite;

    ret = ut_run(suite);

    ut_report(buf, ARRAY_SIZE(buf), suite, true);
#ifdef __epiphany__
    memcpy((void *) epiphany_results, buf, sizeof(buf));
    epiphany_status->returncode = ret;
    epiphany_status->done = 1;
#else
    printf("%s", buf);
#endif

#if 0
    p = trivial_out;
    for ( i = 0; i < TRIVIAL_ROWS - 2; i++ ) {
        for ( j = 0; j < TRIVIAL_COLS - 2; j++ ) {
            printf("%f\t", *p++);
        }
        printf("\n");
    }
#endif

    return ret;
}
