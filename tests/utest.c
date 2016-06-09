/*
 * µTest: a µTiny unit testing framework
 *
 * Copyright (C) 2015 Adapteva, Inc.
 * Author: Ola Jeppsson
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include "utest.h"

static struct ut_tcase *__ut_curr_tcase = NULL;

void __ut_assert_msg(const char *file, const char *func, int line,
                     const char *str, const char *format, ...)
{
    int n;
    va_list args;

    if (!__ut_curr_tcase)
        exit(UT_HARD_ERROR);

#ifndef UT_NO_PRINTF
    n = snprintf(__ut_curr_tcase->msg, UT_TCASE_LOG_LEN, "%s:%s():%d: %s\n",
                 file, func, line, str);

    if (n > UT_TCASE_LOG_LEN)
        exit(UT_HARD_ERROR);

    va_start(args, format);
    /* Don't care if string gets truncated */
    vsnprintf(&__ut_curr_tcase->msg[n], UT_TCASE_LOG_LEN - n, format, args);
    va_end(args);
#endif
}

void ut_log_msg(const char *format, ...)
{
    va_list args;

    if (!__ut_curr_tcase)
        exit(UT_HARD_ERROR);

#ifndef UT_NO_PRINTF
    va_start(args, format);
    /* Don't care if string gets truncated */
    vsnprintf(__ut_curr_tcase->msg, UT_TCASE_LOG_LEN, format, args);
    va_end(args);
#endif
}

int ut_run(struct ut_suite *suite)
{
    int rc = 0;
    struct ut_tcase **p;

    if (suite->setup) {
        if (suite->setup(suite)) {
            rc = UT_HARD_ERROR;
            goto out;
        }
    }

    for (p = suite->tcases; *p; p++)
        suite->ntot++;

    for (p = suite->tcases; *p; p++) {
        __ut_curr_tcase = *p;

        if (!(__ut_curr_tcase->execute || __ut_curr_tcase->verify)) {
            suite->nharderror++;
            __ut_curr_tcase->status = UT_HARD_ERROR;
            ut_log_msg("%s: Testcase lacks both execute() and verify()",
                       __ut_curr_tcase->name);
            continue;
        }

        suite->nrun++;

        if (__ut_curr_tcase->execute) {
            __ut_curr_tcase->status =
                __ut_curr_tcase->execute(suite, __ut_curr_tcase);
            switch (__ut_curr_tcase->status) {
            case UT_PASS:
                break;
            case UT_FAIL:
                suite->nfail++;
                goto check_independent;
            case UT_SKIP:
                suite->nskip++;
                goto check_independent;
            default:
                suite->nharderror++;
                goto check_independent;
            }
        }

        if (__ut_curr_tcase->verify) {
                __ut_curr_tcase->status =
                    __ut_curr_tcase->verify(suite, __ut_curr_tcase);
        }
        switch (__ut_curr_tcase->status) {
        case UT_PASS:
            suite->npass++;
            break;
        case UT_FAIL:
            suite->nfail++;
            break;
        case UT_SKIP:
            suite->nskip++;
            break;
        default:
            suite->nharderror++;
            goto out;
        }

check_independent:
        if (!suite->independent) {
            if (__ut_curr_tcase->status != UT_PASS) {
                rc = __ut_curr_tcase->status;
                break;
            }
        }
    }

    if (suite->nharderror)
        rc = UT_HARD_ERROR;
    else if (suite->nfail)
        rc = UT_FAIL;

    if (suite->teardown) {
        if (suite->teardown(suite)) {
            rc = UT_HARD_ERROR;
            goto out;
        }
    }

out:
    __ut_curr_tcase = NULL;

    return rc;
}
