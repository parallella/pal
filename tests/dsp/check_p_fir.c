/*
 * Test program for dsp: p_fir function
 */

#include <stdio.h>
#include <time.h>
#include <math.h>
#include <pal.h>

#include "fir_test_data.h"

#define EPS  0.0001

int main()
{
    float error1, error2;
    volatile int i;         /* Prevent loop optimization */
    clock_t tic, toc;
    double time_elapsed;

    /*** Test 1 ***/
    /* Test 1: timing */
    tic = clock();
    for (i = 0; i < 10000000; i++) {
        p_fir_f32(x1, h1, r1, nx1, nh1);
    }
    toc = clock();
    time_elapsed = (double)(toc - tic) / CLOCKS_PER_SEC / 10;

    /* Test 1: compute total absolute error */
    error1 = 0.0;
    for (i = 0; i < 32; i++) {
        error1 += fabs(r1[i] - ref1[i]);
    }

    /* Test 1: results */
    printf("Test 1 - Time: %f us.\n", time_elapsed);
    printf("Test 1 - Error = %f.\n\n", error1);


    /*** Test 2 ***/
    /* Test 2: timing */
    tic = clock();
    for (i = 0; i < 10000000; i++) {
        p_fir_f32(x2, h2, r2, nx2, nh2);
    }
    toc = clock();
    time_elapsed = (double)(toc - tic) / CLOCKS_PER_SEC / 10;

    /* Test 2: compute total absolute error */
    error2 = 0.0;
    for (i = 0; i < nx2; i++) {
        error2 += fabs(r2[i] - ref2[i]);
    }

    /* Test 2: results */
    printf("Test 2 - Time: %f us.\n", time_elapsed);
    printf("Test 2 - Error = %f < 1e-4.\n\n", error2);

    if ( (error1 == 0) & (error2 < EPS) ) {
        printf("p_fir is OK!\n");
    } else {
        printf("p_fir test FAILED!\n");
    }

    return(0);
}
