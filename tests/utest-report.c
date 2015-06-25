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

int ut_report(char *buf, size_t n, struct ut_suite *suite, bool verbose)
{
    size_t s, i = 0;
    struct ut_tcase **p, *tcase;
    char *status;
    int proc = 0;

    if (suite->ntot - suite->nskip)
        proc = (100 * (suite->npass)) / (suite->ntot - suite->nskip);

    s = snprintf(&buf[i], n - i,
                 "Suite: %s\n"
                 "%3d%%: Testcases: %d, Pass: %d, Skip: %d, Fail: %d, Hard errors: %d, Not run: %d\n",
                 suite->name, proc, suite->ntot, suite->npass,
                 suite->nskip, suite->nfail, suite->nharderror,
                 suite->ntot - suite->nrun);
    if (s > n - i)
        goto oom;
    i += s;

    if (!verbose)
        return 0;

    for (p = suite->tcases; *p; p++) {
        tcase = *p;

        s = snprintf(&buf[i], n - i, "\n");
        if (s > n - i)
            goto oom;
        i += s;

        switch (tcase->status) {
        case UT_PASS: status = "PASS"; break;
        case UT_FAIL: status = "FAIL"; break;
        case UT_SKIP: status = "SKIP"; break;
        default:      status = "ERR" ; break;
        }
        s = snprintf(&buf[i], n - i, "%s: %s\n", tcase->name, status);
        if (s > n - i)
            goto oom;
        i += s;

        if (tcase->msg[0] == '\0')
            continue;

        s = snprintf(&buf[i], n - i, "%s\n", tcase->msg);
        if (s > n - i)
            goto oom;
        i += s;


    }

    return 0;

oom:
    buf[0] = 'N';
    buf[1] = 'O';
    buf[2] = 'M';
    buf[3] = 'E';
    buf[4] = 'M';
    buf[5] = 'r';
    buf[6] = '\n';
    buf[7] = '\0';
    return ENOMEM;
}
