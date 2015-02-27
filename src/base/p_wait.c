#include <pal.h>

/**
 *
 * Wait for all processes in team to finish.
 *
 * @param team  Pointer to object containing information about team.
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

int p_wait(p_team_t team)
{
    struct team *pteam = (struct team *) team;
    printf("Running p_wait\n");

    if (p_ref_is_err(team))
        return -EINVAL;

    return pteam->dev->dev_ops->wait(pteam->dev, pteam);
}
