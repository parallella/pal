#include <pal.h>

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
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
            p_error(pteam);

    if (pteam->dev->dev_ops->close)
	    return pteam->dev->dev_ops->close(pteam->dev, pteam);

    return 0;
}
