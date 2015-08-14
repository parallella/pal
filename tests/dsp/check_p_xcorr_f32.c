/*
Host program for testing of DSP functions in PAL DSP lib

Currently, the test is only using Arm

*/

#include <utest.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <pal.h>
#include <common.h>

#include "xcorr_test_data_1.h"
#include "xcorr_test_data_2.h"
#include "xcorr_test_data_3.h"

#define MAX_REL_DIFF  0.00001
#define OK 1
#define NOK 0

float test_out1[ARRAY_SIZE(out1)];
float test_out2[ARRAY_SIZE(out2)];
#if 0
// This test is not used, since the code today requires nx >= ny
float test_out3[ARRAY_SIZE(out3)];
#endif

int check_data(float tst, float ref, float max_diff)
{
  float diff = fabs(tst - ref);

  if ( diff == 0 ) return OK;

  if ( ref != 0 ) {
    float rel_diff = fabs(diff/ref);
    if ( rel_diff > max_diff ) {
      return NOK;
    } else {
      return OK;
    }
  } else { // ref = 0, now we can use absolute diff
    if ( diff > max_diff ) {
      return NOK;
    } else {
      return OK;
    }
  }
}

int tc_against_gold_e(struct ut_suite *suite, struct ut_tcase *tcase)
{
    // Run test 1
    p_xcorr_f32(in11, in12, test_out1, in11_size, in12_size);

    // Run test 2
    p_xcorr_f32(in21, in22, test_out2, in21_size, in22_size);

#if 0
    // This test is not used, since the code today requires nx >= ny
    // Run test 3
    p_xcorr_f32(in31, in32, test_out3, in31_size, in32_size);
#endif

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < ARRAY_SIZE(out1); i++) {
        ut_assert_msg(check_data(test_out1[i], out1[i], MAX_REL_DIFF) == OK,
                      "p_xcorr_f32() Test 1: Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, out1[i], test_out1[i], test_out1[i]/out1[i]);
    }

    for (i = 0; i < ARRAY_SIZE(out2); i++) {
        ut_assert_msg(check_data(test_out2[i], out2[i], MAX_REL_DIFF) == OK,
                      "p_xcorr_f32() Test 1: Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, out2[i], test_out2[i], test_out2[i]/out2[i]);
    }

#if 0
    //  This test is not used, since the code today requires nx >= ny
    for (i = 0; i < ARRAY_SIZE(out3); i++) {
        ut_assert_msg(check_data(test_out3[i], out3[i], MAX_REL_DIFF) == OK,
                      "p_xcorr_f32() Test 1: Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, out3[i], test_out3[i], test_out3[i]/out3[i]);
    }
#endif

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
DECLARE_UT_SUITE(p_xcorr_f32_suite, setup, teardown, false, tcases, NULL);

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

    struct ut_suite *suite = &p_xcorr_f32_suite;

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
