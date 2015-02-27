#include <pal.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "pal_base.h"
#include "pal_base_private.h"

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
 * @return          Returns a reference. Negative value indicates error.
 *
 */
p_prog_t p_load(p_dev_t dev, char *file, char *function, int flags)
{
    size_t len;
    struct prog *prog;

    if (p_ref_is_err(dev))
        return p_ref_err(EINVAL);

    prog = malloc(sizeof(*prog));
    if (!prog)
        return p_ref_err(ENOMEM);

    len = strnlen(file, 4096);
    if (len == 4096)
        return p_ref_err(ENAMETOOLONG);

    prog->path = strndup(file, len);;

    if (!prog->path)
        return p_ref_err(ENOMEM);

    strncpy(prog->path, file, len+1);

    return prog;

    // TODO: Load into memory etc.
    // TODO: Add to global prog table. We're leaking memory here people.
}
