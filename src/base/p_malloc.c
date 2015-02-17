#include <stddef.h>
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Dynamically allocates contiguous memory buffer on the local node.
 *
 * @param team  A pointer to the object defining the team structure.
 *              The team contains a set of compute nodes with each node having
 *              a finite amount of memory.
 *
 * @param size  Total amount of memory to allocate
 *
 * @return      Returns a pointer to the memory buffer
 *              Returns NULL on error
 */
p_mem_t p_malloc(p_team_t team, size_t size)
{

    printf("Running p_malloc(%p,%d)\n", team, (int)size);
#if 0
    p_mem_t *mem;

    switch (0) {   // FIX!
    case P_DEV_EPIPHANY: // shared memory model
        break;
    case P_DEV_FPGA: // shared memory model
        break;
    case P_DEV_GPU: // shared memory model
        break;
    case P_DEV_SMP: // heap (thread model)
    /*
    mem=malloc(sizeof(p_mem_t));
    mem->mutex  = 23;
    mem->takeit = 0;
    mem->gotit  = 0;
    mem->size   = size;
    mem->memptr = malloc(size);
    break;
    */
    case P_DEV_GRID: // file+IP (rcp?)
        break;
    default:
        return (1);
    }
#endif
    return p_ref_err(ENOSYS);
}
