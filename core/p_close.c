/**
 *
 * Closes down a team, freeing any allocated resources.
 *
 * @param team  Index of team object
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_close(int team)
{
    printf("Running p_close (%d)\n", team);
    return (0);
}
