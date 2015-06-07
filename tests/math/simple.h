#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>

#include <check.h>
#include <pal.h>
#include "../../src/base/pal_base_private.h"
#include <common.h>

/* Max allowed diff against expected value */
#define EPSILON_MAX 0.001f
#define EPSILON_RELMAX 0.00001f

struct gold {
    float ai;
    float bi;
    float res;
    float gold;
};

extern float *ai, *bi, *res;


/* Functions that can be overridden by individual tests */

/* Compare two values */
bool compare(float x, float y);

/* Allow individual tests to add more test cases, e.g. against a reference
 * function */
void add_more_tests(TCase *tcase);
