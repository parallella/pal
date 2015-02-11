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
 * @param mem   Pointer to a memory object created with p_malloc();
 *
 * @return      Returns 0 if successful.
 *
 */

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "pal_core.h"
#include "pal_core_private.h"
ssize_t p_write(int mem, const void *src, size_t nb, int flags)
{
    printf("Running p_write(%d,%p,%d,%d)\n", mem, src, (int)nb, flags);
    // memcpy(mem->memptr,src,nb);
    return (0);
}
