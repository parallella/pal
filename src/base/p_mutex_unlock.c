#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_unlock(p_mutex_t *m)
{
	__sync_lock_release (m);
	return 0;
}
