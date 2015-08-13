/*
 * Test program for dsp: p_fir function
 */

#include <utest.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <math.h>
#include <pal.h>
#include <common.h>

#include "fir_test_data.h"

/* Returns true on success */
bool compare1(float res, float ref)
{
    return (fabs(res - ref) == 0.0f);
}

/* Returns true on success */
bool compare2(float res, float ref)
{
    return (EPS >= fabs(res - ref));
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    p_fir_f32(x1, h1, r1, nx1, nh1);

    p_fir_f32(x2, h2, r2, nx2, nh2);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < nx1; i++) {
        ut_assert_msg(compare1(r1[i], ref1[i]),
                      "p_fir_f32(): Test 1: Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, ref1[i], r1[i], r1[i]/ref1[i]);
    }

    for (i = 0; i < nx2; i++) {
        ut_assert_msg(compare2(r2[i], ref2[i]),
                      "p_fir_f32(): Test 2: Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, ref2[i], r2[i], r2[i]/ref2[i]);
    }

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
DECLARE_UT_SUITE(p_fir_f32_suite, setup, teardown, false, tcases, NULL);

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

    struct ut_suite *suite = &p_fir_f32_suite;

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
