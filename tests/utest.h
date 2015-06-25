/*
 * µTest: a µTiny unit testing framework
 *
 * Copyright (C) 2015 Adapteva, Inc.
 * Author: Ola Jeppsson
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#pragma once
#include <stdbool.h>
#include <stdlib.h>

/* https://www.gnu.org/software/automake/manual/html_node/Scripts_002dbased-Testsuites.html */
#define UT_PASS        0
#define UT_FAIL        1
#define UT_SKIP       77
#define UT_HARD_ERROR 99

/* Max total assert msg length. */
#define UT_TCASE_LOG_LEN 255

struct ut_tcase;
struct ut_suite;

struct ut_tcase {
    char *name;
    int (*execute)(struct ut_suite *, struct ut_tcase *);
    int (*verify)(struct ut_suite *, struct ut_tcase *);
    void *data;

    /* Read-only for user */
    char msg[UT_TCASE_LOG_LEN + 1];
    int status;
};
#define DECLARE_UT_TCASE(_Name, _Execute, _Verify, _Data)\
struct ut_tcase _Name = {\
    .name    = #_Name,\
    .execute = _Execute,\
    .verify  = _Verify,\
    .data    = _Data\
}

#define DECLARE_UT_TCASE_LIST(_Name, ...)\
struct ut_tcase *_Name[] = {\
    __VA_ARGS__,\
    NULL\
}

struct ut_suite {
    char *name;
    int (*setup)(struct ut_suite *);
    int (*teardown)(struct ut_suite *);
    bool independent; /* True if test cases are independent */
    struct ut_tcase **tcases; /* NULL terminated list */
    void *data;

    /* Read-only for user */
    int npass;
    int nfail;
    int nskip;
    int ntot;
    int nrun;
    int nharderror;
};
#define DECLARE_UT_SUITE(_Name, _Setup, _Teardown, _Independent, \
                         _Tcases, _Data)\
struct ut_suite _Name = {\
    .name        = #_Name,\
    .setup       = _Setup,\
    .teardown    = _Teardown,\
    .independent = _Independent,\
    .tcases      = _Tcases,\
    .data        = _Data,\
}

extern int ut_run(struct ut_suite *suite);
extern int ut_report(char *buf, size_t n, struct ut_suite *suite, bool verbose);
extern void __ut_assert_msg(const char *file, const char *func, int line,
                            const char *str, const char *format, ...);

#define ut_assert(_Expr) ut_assert_msg((_Expr), NULL)
#define ut_assert_msg(_Expr, ...)\
    do {\
        if (!(_Expr)) {\
            __ut_assert_msg(__FILE__, __func__, __LINE__,\
                            "Assertion: '" #_Expr "' failed.", __VA_ARGS__);\
            return UT_FAIL;\
        }\
    } while (0)
