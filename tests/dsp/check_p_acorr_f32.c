/*
Host program for testing of DSP functions in PAL DSP lib

This program is prepared to call one or several Epiphany cores to run the test.

*/


#include <utest.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <pal.h>
#include <common.h>
#include "acorr_test_data.h"

#define MAX_REL_DIFF  0.0001
#define OK 1
#define NOK 0

float test_out[ARRAY_SIZE(out)];

int check_data(float tst, float ref, float max_diff)
{
  float diff = fabs(tst - ref);

  if ( diff == 0 ) return OK;

  if ( ref != 0 ) {
    float rel_diff = diff/ref;
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
    p_acorr_f32(in, test_out, in_size, out_size);

    return 0;
}

int tc_against_gold_v(struct ut_suite *suite, struct ut_tcase *tcase)
{
    size_t i;
    for (i = 0; i < out_size; i++) {
        ut_assert_msg(check_data(test_out[i], out[i], MAX_REL_DIFF) == OK,
                      "p_acorr_f32(): Large diff for index: %d, ref: %f, test: %f, rel: %f\n",
                      i, out[i], test_out[i], test_out[i]/out[i]);
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
DECLARE_UT_SUITE(p_acorr_f32_suite, setup, teardown, false, tcases, NULL);

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

    struct ut_suite *suite = &p_acorr_f32_suite;

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
