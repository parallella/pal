#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_unlock(p_mutex_t *mutex)
{
    struct team *pteam = p_to_team(mutex->team);

    if (p_error(pmutex))
        return p_error(pmutex);

    return pteam->dev->dev_ops->mutex_unlock(pteam, mutex);
}
