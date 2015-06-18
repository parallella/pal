#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

/* Clean up and destroy a p_mutex_t
 * @param mp A pointer to a p_mutex_t.
 * @returns 0 on success,
 *          EINVAL when mp is NULL or uninitialized.
 *          EBUSY when the mutex has not been unlocked beforehand.
 */
int p_mutex_destroy(p_mutex_t *mp)
{
	/*
	 * Conditions:
	 * mp must NOT be NULL (it is a pointer pointer!)
	 * *mp must NOT be NULL (that would mean it hasn't been initialized yet)
	 * **mp must NOT be Non-Zero (that would mean we're trying to unlock a locked mutex.)
	 */

	// Check if mp or *mp are null.
	// If they are not null, check that the mutex is not locked.
	if(mp == NULL || *mp == NULL) {
		return EINVAL;
	}
	else if(**mp !=0) {
		return EBUSY;
	}

	// at this point, the given value should be fine.
	free(*mp);
	// Be nice and clean the part of memory that we're touching.
	*mp = NULL;
	return 0;
}
