/**
 *
 * Loads a program from a file into an object in memory and prepares the program
 * for execution.
 *
 * @param dev       Pointer to object containing device information
 * @param file      File name of executable to load.
 * @param function  Name of function within 'prog' to run
 * @param flags     Optional flags
 *
 * @return          Returns 0 if successful. Negative value indicates error.
 *
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "pal_core.h"
#include "pal_core_private.h"
int p_load (p_dev_t *dev, p_program_t *prog, char *file, char *function,
        int flags)
{

#if _P_DEBUG
    printf("Running p_load(%d,%s,%s,%d)\n",dev,file, function, flags);
#endif

#if 0
    /* We don't own the string so copy */
    prog->name = strdup(file);

    if (!prog->name)
        return -ENOMEM;

    /* Creating a program structure */
    prog->dev = dev;

    dev->progs.tail->next = prog;
    dev->progs.tail = prog;
    prog->next = NULL;


    return 0;
#endif

    return -ENOSYS;

}

