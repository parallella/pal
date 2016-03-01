#include <pal.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Get a raw pointer to a team member. Not all devices can implement this.
 * It is okay to return NULL.
 *
 * @param team      Team
 * @param member    Member
 * @param off       Offset
 * @param size      Size
 *
 * @return          Raw pointer or NULL on error or not supported.
 *
 */
void *p_map_member(p_team_t team, int member, unsigned long off,
                   unsigned long size)
{
    struct team *pteam = (struct team *) team;

    if (p_ref_is_err(team))
        return NULL;

    if (!pteam->dev || !pteam->dev->dev_ops || !pteam->dev->dev_ops->map_member)
        return NULL;

    return pteam->dev->dev_ops->map_member(pteam, member, off, size);
}
