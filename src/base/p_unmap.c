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
int p_unmap(p_team_t team, p_mem_t *mem)
{
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
        return p_error(pteam);

    if (p_mem_error(mem))
        return p_mem_error(mem);

    if (!pteam->dev || !pteam->dev->dev_ops || !pteam->dev->dev_ops->map_member)
        return -ENODEV;

    return pteam->dev->dev_ops->unmap(pteam, mem);
}
