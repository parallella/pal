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
#include "pal_base.h"
#include "pal_base_private.h"

int p_close(p_team_t team)
{
    printf("Running p_close (%p)\n", team);

    if (p_ref_is_err(team))
            return -EINVAL;


    // NO-OP for now

    return 0;
}
