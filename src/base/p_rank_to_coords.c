#include <pal.h>
#include "pal_base_private.h"

/**
 *
 * Convert rank to coordinates
 *
 * @param team     The team.
 * @param rank
 * @param coords   Output coords
 * @param flags
 *
 * @return      Return zero on success. Negative on error.
 *
 */
int p_rank_to_coords(p_team_t team, int rank, p_coords_t *coords, int flags)
{
    struct team *pteam = p_to_team(team);

    if (p_error(pteam))
        return p_error(pteam);

    /* TODO: Range checking */
    switch (pteam->topology) {
    case P_TOPOLOGY_FLAT:
        coords->id = rank;
        break;
    case P_TOPOLOGY_3D:
        coords->plane = rank / (pteam->size.row * pteam->size.col);
	rank = rank % pteam->size.row * pteam->size.col;
        /* Fall through */
    case P_TOPOLOGY_2D:
        coords->row = rank / pteam->size.col;
        coords->col = rank % pteam->size.col;
        break;
    default:
        return -EINVAL;
    }

    return 0;
}
