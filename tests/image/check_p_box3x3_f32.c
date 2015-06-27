#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <pal.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

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


int main(int argc, char *argv[])
{
    // Stack variables
    int i, j;
    float test_out[(COLS - 2) * (ROWS - 2) + 1];
    float *p;
    int testFail = 0;

    *(int*)(&test_out[ARRAY_SIZE(test_out) - 1]) = CANARY;

    p_box3x3_f32(in, test_out, ROWS, COLS);

#if 0
    p = test_out;
    for ( i = 0; i < ROWS - 2; i++ ) {
        for ( j = 0; j < COLS - 2; j++ ) {
            printf("%f\t", *p++);
        }
        printf("\n");
    }
#endif

    for (i = 0; i < ARRAY_SIZE(expected); i++) {
        if (fabs(expected[i] - test_out[i]) > MAX_ABS_DIFF) {
            testFail = 1;
        }
    }

    if (*(int*)(&test_out[ARRAY_SIZE(test_out) - 1]) != CANARY) {
        testFail = 1;
    }

    if (testFail) {
      printf("p_box3x3_f32 test FAILED!!\n");
    } else {
      printf("p_box3x3_f32 test OK\n");
    }

    return testFail;
}
