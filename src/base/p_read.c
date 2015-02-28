#include <pal.h>

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Copies the content of 'mem' into the area pointed to by the 'dst' pointer.
 *
 *
 * @param mem      Pointer to a memory object created with p_malloc();
 *
 * @param dst      Address to copy the memory block to
 *
 * @param offset   Offset (in bytes) relative to start of buffer.
 *
 * @param nb       Number of bytes to copy
 *
 * @param flags    Bitmask field indicating runtime options
 *                 ASYNC: Specifies asynchronous (non-blocking) operation.
 *                 The default is blocking.
 *
 *
 * @return      Returns number of bytes read. Negative value indicates error.
 *
 */
ssize_t p_read(p_mem_t mem, void *dst, off_t offset, size_t nb, int flags)
{
    printf("Running p_read(%p,%d,%d,%p)\n", mem, (int)nb, flags, dst);
    // memcpy(dst, mem->memptr,nb);
    return -ENOSYS;
}
