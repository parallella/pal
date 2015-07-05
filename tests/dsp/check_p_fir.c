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
    int testFail = 0;


    /*** TEST 1 ***/
    p_fir_f32(x1, h1, r1, nx1, nh1);

    /* Compute maximum absolute error */
    error1 = 0.0;
    for (i = 0; i < 32; i++) {
        if (error1 < fabs(r1[i] - ref1[i]))
            error1 = fabs(r1[i] - ref1[i]);
    }

    if (error1 != 0) {
        printf("p_fir: Test 1 - Maximum error = %f is not zero.\n\n", error1);
        testFail = 1;
    }


    /*** TEST 2 ***/
    p_fir_f32(x2, h2, r2, nx2, nh2);

    /* Compute total absolute error */
    error2 = 0.0;
    for (i = 0; i < nx2; i++) {
        if (error2 < fabs(r2[i] - ref2[i]))
            error2 = fabs(r2[i] - ref2[i]);
    }

    if (error2 > EPS) {
        printf("p_fir: Test 2 - Maximum error = %f < %.1e.\n\n", error2, EPS);
        testFail = 1;
    }


    /*** RESULTS ***/
    if ( testFail == 0 ) {
        printf("p_fir is OK!\n");
    } else {
        printf("p_fir test FAILED!\n");
    }

    return(testFail);
}
