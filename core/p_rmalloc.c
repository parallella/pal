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
 * @return      Returns a pointer to the memory buffer
 *              Returns NULL on error
 */
#include <stddef.h>
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

void *p_rmalloc(int team, int pid, size_t size)
{
    printf("Running p_rmalloc(p_team_t,%d,%d)\n", pid, (int)size);
    return (mem);
}
