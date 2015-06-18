#include <pal.h>

/*
 *
 * FUNCTION:
 * p_copy(): copies an array of bytes from source to destination
 *
 * ARGUMENTS:
 * dst   - complete address destination pointer
 * src   - complete address source pointer
 * nb    - number of bytes to copy
 * flags - independent flags to assert, separate with '|'
 *         ASYNC : Makes copy call asynchronous (ie non-blocking)
 *
 * NOTES:
 * 0. User space
 * 1. Specific to bsb/device/os
 * 2. Should be lightning fast
 * 3. Add "safety" compile switch for error checking
 * 4. Function should not call any
 * 5. Need a different call per chip, board, O/S?
 *
 */
#include <stddef.h>
#ifndef FAIL_FAST
#include <assert.h>
#endif
#include "pal_base.h"
#include "pal_base_private.h"

static p_mutex_t __global_memset_mutex = 0;

#ifndef __OWN_MEMCPY__
#define __OWN_MEMCPY__
ssize_t p_memcpy(void *dst, const void *src, size_t nb, int flags)
{
#ifdef FAIL_FAST
    if(src+nb >= dst) {
        printf("Bad p_memcpy in %s (dst=0x%8x src=0x%8x, nb=%d)",
            __FILE__,
            dst,
            src,
            nb);
            
        abort(); }
#else
    assert(src+nb < dst);
#endif
    int idx;
		// Take the mutex if we've not been asked to ignore it.
    if(!(flags && P_FLAG_ASYNC)) {
        p_mutex_lock(&__global_memset_mutex);
    }

    for(idx = 0; idx < nb; idx++) {
        *((uint8_t*)(dst+idx)) = *((uint8_t*)(src+idx));
    }
    // Relinquish the mutex that we took earlier. 
    if(!(flags && P_FLAG_ASYNC)) {
      p_mutex_unlock(&__global_memset_mutex);
    }
    return (0);
}
#endif
