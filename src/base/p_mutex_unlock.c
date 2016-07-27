#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_unlock(p_mutex_t *mutex)
{
    struct team *pteam = _p_unwrap_team(mutex->team);

    return pteam->dev->dev_ops->mutex_unlock(pteam, mutex);
}
