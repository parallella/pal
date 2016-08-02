#include <pal.h>
#include "pal_base_private.h"

/**
 *
 * Get size of team
 *
 * @param team  The team.
 *
 * @return      Return size of team. Negative result on error.
 *
 */
int p_team_size(p_team_t team)
{
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
        return p_error(pteam);

    switch (pteam->topology) {
        case P_TOPOLOGY_FLAT:
            return pteam->size.id;
        case P_TOPOLOGY_2D:
            return pteam->size.row * pteam->size.col;
        case P_TOPOLOGY_3D:
            return pteam->size.plane * pteam->size.row * pteam->size.col;
        default:
            return -EINVAL;
    }
    return -EINVAL;
}
