#include <pal.h>
#include "pal_base_private.h"

/**
 *
 * Get this members rank in the team
 *
 * @param team  The team.
 *
 * @return      Return this members rank in the team. Negative value indicates
 *              error.
 *
 */
int p_team_rank(p_team_t team)
{
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
        return p_error(pteam);

    switch (pteam->topology) {
        case P_TOPOLOGY_FLAT:
        return pteam->rank.id;
        case P_TOPOLOGY_2D:
        case P_TOPOLOGY_3D:
            /* TODO: Support more than flat topology */
            return -ENOSYS;
        default:
            return -EINVAL;
    }
    return -EINVAL;
}
