/**
 *
 * Runs(launches) the program 'prog' on all of the members of 'team'.
 *
 * @param prog      Pointer to the program to run that was loaded with p_load();
 *
 * @param team      Team to run with.
 *
 * @param start     Relative starting processor within team
 *
 * @param size      Total number of processors within team to run
 *
 * @param argn      Number of arguments to be supplied to 'function'
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

int p_run(p_prog_t prog, p_team_t team, int start, int size, int argn,
        char *args[], int flags)
{

    /* TODO: Clean me up please */

    printf("Running p_run(%p,%p,%d,%d,%d, argv,%d)\n", prog, team, start,
            size, nargs, flags);
#if 0
    struct p_prog *progptr = p_prog_table_global.progptr[prog];
    p_team_t *teamptr = p_team_table_global.teamptr[team];
    p_dev_t *devptr = progptr->devptr;
    int type = devptr->property[TYPE];

    pid_t child_pid[16]; // FIX!!
    int wpid, i, status = 0;

    char *const elf[] = {progptr->name, NULL};
    char *path = progptr->name;

    char *argv[16]; // FIX!
    switch (type) {
    case P_DEV_EPIPHANY:
    case P_DEV_FPGA:
    case P_DEV_GPU:
    case P_DEV_SMP:
    case P_DEV_GRID:
        break;
    case P_DEV_DEMO:
        for (i = 0; i < nargs; i++) {
            argv[i] = args[i];
        }
        for (i = nargs; i < 16; i++) {
            argv[i] = NULL;
        }
        for (i = start; i < (size + start); i++) {
            child_pid[i] = fork();
            if (child_pid[i] == 0) {
                execve(path, elf, argv); // executing
                exit(0);
            }
        }
        // Waiting for all children to finish. Right way?
        for (i = start; i < (start + size); i++) {
            while (0 < waitpid(child_pid[i], NULL, 0))
                ;
        }
        break;
    default:
        return -ENOSYS;
    }
#endif
    return -ENOSYS;
}
