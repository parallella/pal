#pragma once
#include <pal.h>
#include <stdbool.h>
#include <stdlib.h>

/* Max allowed diff against expected value */
#ifndef EPSILON_MAX
#define EPSILON_MAX ((PTYPE)0.001)
#endif
#ifndef EPSILON_RELMAX
#define EPSILON_RELMAX ((PTYPE)0.00001)
#endif

struct gold {
    PTYPE ai;
    PTYPE bi;
    PTYPE res;
    PTYPE gold1;
    PTYPE gold2;
};

extern PTYPE *ai, *result1, *result2;
extern int *resultIndex1, *resultIndex2;

/* Functions that can be overridden by individual tests */

/* Compare two values */
bool compare(PTYPE x, PTYPE y);

/* Needs to be implemented by tests that define FUNCTION */
void generate_ref(PTYPE *outValue1, PTYPE *outValue2, int *outIndex1, int *outIndex2, size_t n);
