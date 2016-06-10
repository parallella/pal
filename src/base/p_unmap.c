#include <pal.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Unmap the memory region of team member.
 *
 * @param team      Team
 * @param addr      Address returned by previous call to p_map_member().
 *
 * @return          Non-zero on error.
 *
 */
int p_unmap(p_team_t team, void *addr)
{
    struct team *pteam = (struct team *) team;

    if (p_ref_is_err(team))
        return -EINVAL;

    if (!pteam->dev || !pteam->dev->dev_ops || !pteam->dev->dev_ops->map_member)
        return -ENODEV;

    return pteam->dev->dev_ops->unmap(pteam, addr);
}
