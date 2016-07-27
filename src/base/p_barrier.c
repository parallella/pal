#include <pal.h>

/**
 *
 * A barrier for 'team'. The barrier is initialized when the team is
 * opened.
 *
 * @param team  Pointer to object containing information about team.
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

int p_barrier(p_team_t team)
{
    struct team *pteam = _p_unwrap_team(team);

    return pteam->dev->dev_ops->barrier(pteam);
}
