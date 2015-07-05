/*
 * Test program for dsp: p_fir function
 */

#include <stdio.h>
#include <math.h>
#include <pal.h>

#include "fir_test_data.h"


int main()
{
    float error1, error2;
    int i;


    /*** TEST 1 ***/
    p_fir_f32(x1, h1, r1, nx1, nh1);

    /* Compute maximum absolute error */
    error1 = 0.0;
    for (i = 0; i < 32; i++) {
        if (error1 < fabs(r1[i] - ref1[i]))
            error1 = fabs(r1[i] - ref1[i]);
    }


    /*** TEST 2 ***/
    p_fir_f32(x2, h2, r2, nx2, nh2);

    /* Compute total absolute error */
    error2 = 0.0;
    for (i = 0; i < nx2; i++) {
        if (error2 < fabs(r2[i] - ref2[i]))
            error2 = fabs(r2[i] - ref2[i]);
    }


    /*** RESULTS ***/
    printf("Test 1 - Error = %f.\n\n", error1);
    printf("Test 2 - Error = %f < %.1e.\n\n", error2, EPS);

    if ( (error1 == 0) & (error2 < EPS) ) {
        printf("p_fir is OK!\n");
        return(0);
    } else {
        printf("p_fir test FAILED!\n");
        return(1);
    }
}
