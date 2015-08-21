#pragma once
#include <stdbool.h>
#include <stdlib.h>

/* Max allowed diff against expected value */
#ifndef EPSILON_MAX
#define EPSILON_MAX 0.001f
#endif
#ifndef EPSILON_RELMAX
#define EPSILON_RELMAX 0.00001f
#endif

struct gold {
    float ai;
    float bi;
    float res;
    float gold;
};

extern float *ai, *result;
extern int *resultIndex;

/* Functions that can be overridden by individual tests */

/* Compare two values */
bool compare(float x, float y);

/* Needs to be implemented by tests that define FUNCTION */
void generate_ref(float *outValue, int* outIndex, size_t n);
