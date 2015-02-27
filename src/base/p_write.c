#include <pal.h>

/**
 *
 * Copies a block pointed to by 'src' into the memory objecct
 * 'mem'.
 *
 * @param src   A pointer to a block of memory
 *
 * @param nb    Number of bytes to copy
 *
 * @param flags Bitmask field indicating runtime options
 *
 *        ASYNC: Specifies asynchronous (non-blocking) operation.
 *               The default is blocking
 *
 * @param mem   Reference to a memory object created with p_malloc();
 *
 * @return      Returns 0? if successful. Negative value indicates error.
 *
 */

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "pal_base.h"
#include "pal_base_private.h"
ssize_t p_write(p_mem_t mem, const void *src, size_t nb, int flags)
{
    printf("Running p_write(%p,%p,%d,%d)\n", mem, src, (int)nb, flags);
    // memcpy(mem->memptr,src,nb);
    return -ENOSYS;
}
