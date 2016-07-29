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

    return pteam->rank;
}
