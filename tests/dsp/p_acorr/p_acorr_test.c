/*
Host program for testing of DSP functions in PAL DSP lib

This program is prepared to call one or several Epiphany cores to run the test.

Currently, the test is only using Arm

*/


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/fb.h>
#include <sys/mman.h>
#include <stdint.h>
#include <time.h>
#include <math.h>
#include "pal_base.h"
#include "pal_dsp.h"
#include "acorr_test_data.h"

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
    char *file = "./e_test.elf";
    char *func = "main";
    int status, i, all, nargs = 1;
    char *args[nargs];
    char argbuf[20];
    float test_out[out_size];
    int testOK = 1;

    // References as opaque structures
    p_dev_t dev0;
    p_prog_t prog0;
    p_team_t team0;
    p_mem_t mem[4];

  printf("Running test program p_acorr_test...\n");
  //   printf("   Cycles measured for %d samples\n",in_size);
    // Execution setup
    dev0 = p_init(P_DEV_DEMO, 0);        // initialize device and team
    prog0 = p_load(dev0, file, func, 0); // load a program from file system
    all = p_query(dev0, P_PROP_NODES);   // find number of nodes in system
    team0 = p_open(dev0, 0, all);        // create a team


    // Run the test on Arm
    p_acorr_f32(in, test_out, in_size, out_size,1, team0);

    // Check data
    for ( i = 0; i < out_size; i++ ) {
      if (check_data(test_out[i],out[i], MAX_REL_DIFF) == NOK ) {
	testOK = 0;
	printf("Large diff for index %d, ref:%f   test: %f   rel:%f \n",i,out[i],test_out[i], test_out[i]/out[i]);
      }
    }

    if ( testOK ){
      printf("Acorr ARM test OK\n");
    } else {
      printf("Acorr ARM test FAILED!!\n");
    }
    // Running program
    /* for (i = 0; i < 1; i++) {  // was all */
    /*     sprintf(argbuf, "%d", i); // string args needed to run main asis */
    /*     args[0] = argbuf; */
    /*     status = p_run(prog0, team0, i, 1, nargs, args, 0); */
    /* } */

    p_wait(team0);    // not needed
    p_close(team0);   // close team
    p_finalize(dev0); // finalize memory

}

