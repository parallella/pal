#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

/* attempts to consume the mutex mp
 *
 * This will quickly make an attempt to grab the mutex you are handing it.
 * It makes a cursory check to see that you aren't passing in NULL or
 * an uninitialized mutex.
 *
 * @param mp pointer to a p_mutex_t
 * @returns 0 on success, EINVAL when passed NULL, EBUSY if you lose the race.
 */
int p_mutex_trylock(p_mutex_t *mp)
{
	if( mp == NULL || *mp == NULL) {
		return EINVAL;
	}
	else if(**mp == 0) {
        **mp = 1;
        return 0;
	}
    else {
        return EBUSY;
	}
}
