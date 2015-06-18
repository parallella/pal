#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

/* Take lock of a mutex.
 * 
 * This will make a cursory check that you aren't passing NULL or a pointer
 * to NULL in it. It won't check if you've been an idiot and passed in
 * a uint32_t** because you aren't using the typedef.
 * 
 * This is a blocking call. It will sit, chewing away at CPU time until it
 * gets the mutex. It will constantly dereference the pointer you've handed
 * it.
 * 
 * If you destroy the mutex you are attempting to lock with this while the
 * a call to this is going, you will segfault. If you don't segfault,
 * it will still make no checks to see that you haven't quietly reallocated
 * that bit of memory.
 *
 * @returns 0 on success, EINVAL if you pass it NULL before locking.
 */
int p_mutex_lock(p_mutex_t *mp)
{
	if(mp == NULL || *mp == NULL) {
		return EINVAL;
	}
    // Spin while we wait for the lock to become 0.
    while(**mp != 0) { ;; }
    **mp = 1;
    return (0);
}
