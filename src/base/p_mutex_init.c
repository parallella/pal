#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_init(p_mutex_t *mutex, p_team_t team)
{
    mutex->team = team;
    mutex->mutex = 0;

    return 0;
}
