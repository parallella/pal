#include <pal.h>
#include "pal_base_private.h"

/**
 *
 * Convert relative coordinates to rank
 *
 * @param team     The team.
 * @param coords   Coordinates. Relative to team base.
 *                 Must have same topology as the team.
 * @param flags  flags.
 *
 * @return      Return zero on success. Negative on error.
 *
 */
int p_rel_coords_to_rank(p_team_t team, int rank, const p_coords_t *coords,
                         int flags)
{
    struct team *pteam = p_to_team(team);
    p_coords_t abs_coords;

    if (p_error(pteam))
        return p_error(pteam);

    abs_coords = pteam->rank;

    /* TODO: Overflow + negative relative coords broken */
    switch (pteam->topology) {
    case P_TOPOLOGY_FLAT:
        abs_coords.id += coords->id;
        break;
    case P_TOPOLOGY_3D:
        abs_coords.plane += coords->plane;
        if (flags & P_COORDS_WRAP_PLANE)
            abs_coords.plane = abs_coords.plane % pteam->size.plane;
        /* Fall through */
    case P_TOPOLOGY_2D:
        abs_coords.row += coords->row;
        if (flags & P_COORDS_WRAP_ROW)
            abs_coords.row = abs_coords.row % pteam->size.row;
        if (flags & P_COORDS_WRAP_COL)
            abs_coords.col = abs_coords.col % pteam->size.col;
        break;
    default:
        return -EINVAL;
    }

    return p_coords_to_rank(pteam, &abs_coords, flags & ~P_COORDS_RELATIVE);
}
