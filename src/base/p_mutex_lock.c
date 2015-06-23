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
    /* We've got to avoid race conditions. Our solution to this is to spin
     * until we can increment the mutex and make it 1, not 2.
     * This works because our definition of "locked" is non-zero
     * and our unlock is set to 0.
     * 
     * When we lose the race, our mutex will be a value other than 1.
     *
     * When we enter the loop, we spin until **mp == 0
     * When **mp == 0, we check if ++(**mp) == 1
     * If that condition is true, we are safe: We won the race.
     * If that condition is false, we lost the race: keep trying.
     */
    do {
        /* Spin while the lock is taken. */
        while(**mp != 0) { ;; }
        /* attempt to take the lock. If we lost the race
         * we'll keep trying.
         */
    } while( ++(**mp) != 1);
    return (0);
}
