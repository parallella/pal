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
ssize_t p_write(p_mem_t *mem, const void *src, off_t offset, size_t nb,
                int flags)
{
    struct mem_ops *mem_ops = mem->ops;

    if (p_mem_error(mem))
        return p_mem_error(mem);

    if (!mem_ops || !mem_ops->write)
        return -ENOSYS;

    return mem_ops->write(mem, src, offset, nb, flags);
}
