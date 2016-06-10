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
#include <stdint.h>
#include <stddef.h>
#include "utest.h"


/* ut_utoa:  convert unsigned integer to string */
static unsigned ut_utoa(unsigned n, char *s)
{
    unsigned len = 0, i, j;
    char tmp;

    for (len = 0; n || !len; len++, n /= 10)
        s[len] = n % 10 + '0';

    for (i = 0, j = len - 1; i < j; i++, j--) {
        tmp = s[i];
        s[i] = s[j];
        s[j] = tmp;
    }

    s[len++] = '\0';

    return len;
}

int ut_report(char *buf, size_t n, struct ut_suite *suite, bool verbose)
{
    size_t s, i = 0;
    struct ut_tcase **p, *tcase;
    char *status;
    unsigned proc = 0;
    bool dep_chain_broken = false;

    char sproc[13] = "  ";
    char ntot[11];
    char nrun[11];
    char npass[11];
    char nskip[11];
    char nfail[11];
    char nharderror[11];

    ut_utoa(suite->ntot, ntot);
    ut_utoa(suite->nrun, nrun);
    ut_utoa(suite->npass, npass);
    ut_utoa(suite->nskip, nskip);
    ut_utoa(suite->nfail, nfail);
    ut_utoa(suite->nharderror, nharderror);

    if (suite->ntot - suite->nskip)
        proc = (100 * (suite->npass)) / (suite->ntot - suite->nskip);

    if (proc >= 100)
        ut_utoa(proc, &sproc[0]);
    else if (proc >= 10)
        ut_utoa(proc, &sproc[1]);
    else
        ut_utoa(proc, &sproc[2]);

#define P(str) \
    do { \
        s = strnlen(str, n - i + 1); \
        if (s >= n - i) \
            goto oom; \
        strcpy(&buf[i], str); \
        i += s; \
    } while (0);

    P("Suite: "); P(suite->name); P("\n");
    P(sproc); P("%:");
    P(" Tests: "); P(ntot);
    P(" Run: "); P(nrun);
    P(" Pass: ");P(npass);
    P(" Skip: "); P(nskip);
    P(" Fail: "); P(nfail);
    P(" Hard errors: "); P(nharderror);
    P("\n");

    if (suite->setup_status) {
        P("setup failed\n");
        return 0;
    }

    if (!verbose && !suite->nfail && !suite->nharderror
        && !suite->teardown_status)
        return 0;

    if (suite->teardown_status)
        P("teardown failed\n");

    for (p = suite->tcases; *p; p++) {
        tcase = *p;

        if (dep_chain_broken) {
            status = "NOTRUN";
        } else {
            switch (tcase->status) {
            case UT_PASS: status = "PASS"; break;
            case UT_FAIL: status = "FAIL"; break;
            case UT_SKIP: status = "SKIP"; break;
            default:      status = "ERR" ; break;
            }
        }

        if (!suite->independent && tcase->status != UT_PASS)
            dep_chain_broken = true;

        P(tcase->name); P(": "); P(status); P("\n");

        if (tcase->msg[0] == '\0')
            continue;

        P(tcase->msg); P("\n");
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
