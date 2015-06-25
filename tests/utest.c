/*
 * µTest: a µTiny unit testing framework
 *
 * Copyright (C) 2015 Adapteva, Inc.
 * Author: Ola Jeppsson
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "utest.h"

static struct ut_tcase *__ut_curr_tcase = NULL;

void __ut_assert_msg(const char *file, const char *func, const char *line,
                     const char *str)
{
    char *p, *endp;

    if (!__ut_curr_tcase)
        exit(UT_HARD_ERROR);

    p = __ut_curr_tcase->msg;
    endp = &__ut_curr_tcase->msg[UT_TCASE_LOG_LEN];

    p = strncpy(p, file, endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, ":", endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, func, endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, "():", endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, line, endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, ": ", endp - p);
    if (p >= endp)
        goto oom;

    p = strncpy(p, str, endp - p);
    if (p >= endp)
        goto oom;

    return;

oom:
    __ut_curr_tcase->msg[0] = 'N';
    __ut_curr_tcase->msg[1] = 'O';
    __ut_curr_tcase->msg[2] = 'M';
    __ut_curr_tcase->msg[3] = 'E';
    __ut_curr_tcase->msg[4] = 'M';
    __ut_curr_tcase->msg[5] = 'a';
    __ut_curr_tcase->msg[6] = '\n';
    __ut_curr_tcase->msg[7] = '\0';
}

void ut_log_msg(const char *str)
{
    char *p, *endp;

    if (!__ut_curr_tcase)
        exit(UT_HARD_ERROR);

    p = __ut_curr_tcase->msg;
    endp = &__ut_curr_tcase->msg[UT_TCASE_LOG_LEN];

    strncpy(p, __ut_curr_tcase->name, endp - p);
    if (p >= endp)
        goto oom;

    strncpy(p, ": ", endp - p);
    if (p >= endp)
        goto oom;


    strncpy(p, str, endp - p);
    if (p >= endp)
        goto oom;

    return;

oom:
    __ut_curr_tcase->msg[0] = 'N';
    __ut_curr_tcase->msg[1] = 'O';
    __ut_curr_tcase->msg[2] = 'M';
    __ut_curr_tcase->msg[3] = 'E';
    __ut_curr_tcase->msg[4] = 'M';
    __ut_curr_tcase->msg[5] = 'l';
    __ut_curr_tcase->msg[6] = '\n';
    __ut_curr_tcase->msg[7] = '\0';
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
            ut_log_msg("Testcase lacks both execute() and verify()");
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
