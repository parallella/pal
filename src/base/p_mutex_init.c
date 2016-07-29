#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_init(p_mutex_t *mutex, p_team_t team)
{
    struct team *pteam = p_to_team(mutex->team);

    mutex->team = pteam;
    mutex->mutex = 0;

    if (p_error(pteam))
        return p_error(pteam);

    return 0;
}
