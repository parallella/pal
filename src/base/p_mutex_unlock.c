#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

/*
 * Unlock a mutex.
 *
 * This makes a cursory check to see if you're passing it a bad value.
 *
 * @param mp the mutex to unlock
 * @returns 0 on success, EINVAL if mp is NULL or uninitialized.
 *
 */
int p_mutex_unlock(p_mutex_t *mp)
{
	if(mp == NULL || *mp == NULL) {
		return EINVAL;
	}
	else
	{
    	**mp = 0;
    	return (0);
	}
}
