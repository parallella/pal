/*
Host program for testing of DSP functions in PAL DSP lib

Currently, the test is only using Arm

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <pal.h>

#include "xcorr_test_data_1.h"
#include "xcorr_test_data_2.h"
#include "xcorr_test_data_3.h"

#define MAX_REL_DIFF  0.00001
#define OK 1
#define NOK 0

int check_data(float tst, float ref, float max_diff)
{
  float diff = fabs(tst - ref);

  if ( diff == 0 ) return OK;

  if ( ref != 0 ) {
    float rel_diff = abs(diff/ref);
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

int main(int argc, char *argv[])
{

    // Stack variables
    float test_out[500];
    int testOK = 1;
    int i;

    // References as opaque structures
    p_team_t team0;

  printf("Running test program p_xcorr_test...\n");
    // Execution setup

    // Run test 1 on Arm
    p_xcorr_f32(in11, in12, test_out, in11_size, in12_size, 1, team0);

    // Check data
    for ( i = 0; i < out_size1; i++ ) {
      if (check_data(test_out[i],out1[i], MAX_REL_DIFF) == NOK ) {
	testOK = 0;
	printf("Test 1: Large diff for index %d, ref:%f   test: %f   rel:%f \n",i,out1[i],test_out[i], test_out[i]/out1[i]);
      }
    }

    // Run test 2 on Arm
    p_xcorr_f32(in21, in22, test_out, in21_size, in22_size, 1, team0);

    // Check data
    for ( i = 0; i < out_size2; i++ ) {
      if (check_data(test_out[i],out2[i], MAX_REL_DIFF) == NOK ) {
	testOK = 0;
	printf("Test 2: Large diff for index %d, ref:%f   test: %f   rel:%f \n",i,out2[i],test_out[i], test_out[i]/out2[i]);
      }
    }

    /*  // This test is not used, since the code today requires nx >= ny
    // Run test 3 on Arm
    p_xcorr_f32(in31, in32, test_out, in31_size, in32_size, 1, team0);

    // Check data
    for ( i = 0; i < out_size3; i++ ) {
      if (check_data(test_out[i],out3[i], MAX_REL_DIFF) == NOK ) {
	testOK = 0;
	printf("Test 3: Large diff for index %d, ref:%f   test: %f   rel:%f \n",i,out3[i],test_out[i], test_out[i]/out3[i]);
      }
    }
    */

    if ( testOK ){
      printf("Xcorr ARM test OK\n");
    } else {
      printf("Xcorr ARM test FAILED!!\n");
    }

}

