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

#define COLS 5
#define ROWS 5

float in[COLS * ROWS] = {
    0.0f, 0.2f, 0.4f, 0.6f, 0.8f,
    0.2f, 0.4f, 0.6f, 0.8f, 1.0f,
    1.0f, 0.5f, 1.0f, 0.5f, 1.0f,
    1.0f, 0.8f, 0.6f, 0.4f, 0.2f,
    0.0f, 0.2f, 0.4f, 0.6f, 0.8f,
};

float expected[] = {
0.477778,        0.555556,        0.744444,
0.677778,        0.622222,        0.677778,
0.611111,        0.555556,        0.611111,
};

float test_out[(COLS - 2) * (ROWS - 2) + 1];

/* Returns true on success */
bool compare(float res, float ref)
{
    return (fabs(res - ref) <= MAX_ABS_DIFF);
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    *(int*)(&test_out[ARRAY_SIZE(test_out) - 1]) = CANARY;

    p_box3x3_f32(in, test_out, ROWS, COLS);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < ARRAY_SIZE(expected); i++) {
        ut_assert_msg(compare(test_out[i], expected[i]),
                      "p_box3x3_f32(): Large diff for index: %d, ref: %f, test: %f, diff: %f\n",
                      i, expected[i], test_out[i], expected[i]-test_out[i]);
    }

    ut_assert_msg((*(int*)(&test_out[ARRAY_SIZE(test_out) - 1]) == CANARY),
                  "p_box3x3_f32(): CANARY overwritten\n");

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

DECLARE_UT_TCASE(tc_against_gold, tc_against_gold_e, tc_against_gold_v, NULL);
DECLARE_UT_TCASE_LIST(tcases, &tc_against_gold);
DECLARE_UT_SUITE(p_box3x3_f32_suite, setup, teardown, false, tcases, NULL);

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

    struct ut_suite *suite = &p_box3x3_f32_suite;

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
    p = test_out;
    for ( i = 0; i < ROWS - 2; i++ ) {
        for ( j = 0; j < COLS - 2; j++ ) {
            printf("%f\t", *p++);
        }
        printf("\n");
    }
#endif

    return ret;
}
