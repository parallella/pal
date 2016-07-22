#include <pal.h>

/**
 *
 * Runs(launches) the program 'prog' on all of the members of 'team'.
 *
 * @param prog      Pointer to the program to run that was loaded with p_load()
 *
 * @param function  Name of function within 'prog' to run
 *
 * @param team      Team to run with.
 *
 * @param start     Relative starting processor within team
 *
 * @param size      Total number of processors within team to run
 *
 * @param nargs     Number of arguments to be supplied to 'function'
 *
 * @param args      An array of pointers to function arguments
 *
 * @param flags     Bitfield mask type flags
 *
 * @return          Returns 0 if successful.
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_run(p_prog_t prog, const char *function, p_team_t team,
          int start, int count, int nargs, const p_arg_t *args, int flags)
{
    int err = 0;
    struct team *pteam = (struct team *) team;
    struct prog *pprog = (struct prog *) prog;
    struct dev_ops *ops = pteam->dev->dev_ops;

    if (p_ref_is_err(prog) || p_ref_is_err(team))
        return -EINVAL;

    if (!(flags & P_RUN_PREPARED)) {
        if (nargs > P_RUN_MAX_ARGS)
            return -E2BIG;

        err = ops->load(pteam, start, count, pprog, function, nargs, args);
        if (err)
            return err;
    }

    if (flags & P_RUN_PREPARE)
        return 0;

    err = ops->start(pteam, start, count);
    if (err)
        return err;

    if (!(flags & P_RUN_NONBLOCK))
        err = p_wait(pteam);

    return err;
}
