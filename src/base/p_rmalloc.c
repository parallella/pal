#include <pal.h>

#include <stddef.h>
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Dynamically allocates contiguous memory buffers at any node within
 * the specified 'team' object. All inter-node data communication within a
 * team is done between these dynamically allocated memory blocks. The function
 * returns a pointer to the buffer of allocated memory.
 *
 * @param team  Team object descriptor (int)
 *
 * @param pid   The relative processor id within the 'team' starting at 0.
 *
 * @param size  Total amount of memory to allocate
 *
 * @return      Returns a reference to the memory buffer.
 *              Returns negative value on error.
 */
p_mem_t p_rmalloc(p_team_t team, int pid, size_t size)
{
    printf("Running p_rmalloc(p_team_t,%d,%d)\n", pid, (int)size);
    return p_ref_err(ENOSYS);
}
