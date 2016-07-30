#include <pal.h>
#include "pal_base_private.h"

/**
 *
 * Convert coordinates to rank
 *
 * @param team     The team.
 * @param coords   Coordinates. Relative to team base.
 *                 Must have same topology as the team.
 * @param flags  flags.
 *
 * @return      Return zero on success. Negative on error.
 *
 */
int p_coords_to_rank(p_team_t team, const p_coords_t *coords, int flags)
{
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
        return p_error(pteam);

    switch (pteam->topology) {
    case P_TOPOLOGY_FLAT:
        return coords->id;
    case P_TOPOLOGY_2D:
        return coords->col + coords->row * pteam->size.col;
    case P_TOPOLOGY_3D:
        return coords->col +
               coords->row * pteam->size.col +
               coords->plane * pteam->size.col * pteam->size.row;
    default:
        return -EINVAL;
    }
    return -EINVAL;
}
