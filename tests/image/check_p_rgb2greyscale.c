/*
Host program for testing of DSP functions in PAL DSP lib

This program is prepared to call one or several Epiphany cores to run the test.

Currently, the test is only using Arm

*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <pal.h>
#include "rgb2greyscale_test_data.h"

#define MAX_REL_DIFF  0.0001
#define OK 1
#define NOK 0

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

int main(int argc, char *argv[])
{
    // Stack variables
    int i;
    float test_out[out_size];
    int testOK = 1;

    // Run the test on Arm
    p_rgb2grayscale_f32(in, test_out, in_rows, in_cols);

    // Check data
    for ( i = 0; i < out_size; i++ ) {
      if (check_data(test_out[i],out[i], MAX_REL_DIFF) == NOK ) {
	testOK = 0;
	printf("Large diff for index %d, ref:%f   test: %f   rel:%f \n",i,out[i],test_out[i], test_out[i]/out[i]);
      }
    }

    if ( testOK ){
      printf("RGB2Grayscale ARM test OK\n");
    } else {
      printf("RGB2Grayscale ARM test FAILED!!\n");
    }

}

