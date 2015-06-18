#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

/*
 * Initalize a mutex.
 *
 * This makes no attempt to make sure you're not clobbering an existing mutex.
 * If for some reason you decide to pass this a locked, in-use mutex, it will
 * happily trapse all over it and take your determinism right with it.
 * 
 * This WILL make a cursory check to make sure malloc succeeded.
 * 
 * @returns 0 on success, EINVAL if the p_mutex_t* is bad (NULL)
 * 
 */
int p_mutex_init(p_mutex_t * mp)
{
    p_mutex_t tmp; 
    if(mp == NULL) {
        return EINVAL;
    }
    *mp = malloc(sizeof(**mp));
    if(*mp == NULL) {
        return EINVAL;
    }
    **mp = 0;
}
