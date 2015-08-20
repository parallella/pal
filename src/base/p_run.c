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
          int start, int count, int nargs, const void *args[], int flags)
{
    int err;
    struct team *pteam = (struct team *) team;
    struct prog *pprog = (struct prog *) prog;
    struct dev *pdev;

    if (p_ref_is_err(prog) || p_ref_is_err(team))
        return -EINVAL;

    pdev = pteam->dev;

    err = pdev->dev_ops->run(pdev, pteam, pprog, function, start, count, nargs,
                             args, flags);

    if (err)
        return err;

    if (!(flags & 1)) // Bogus non blocking check
        return p_wait((p_team_t) team); // Inconsistent with flags to this function

    return 0;
}
