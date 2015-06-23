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
    if(**mp !=0 ) { return EBUSY; }
    /* Try to take the mutex. If we won, **mp will be 1. If we lost,
     * it'll be something different.
     * 
     * There's one small caveat to this: If, by some magical means, you have
     * the ability to make UINT_MAX attempts *simultaneously*
     * you run the slight risk of overflowing.
     * 
     * That's a lot of attempts, though.
     */
    return ( ++(**mp) == 1 ? 0 : EBUSY );    
}
