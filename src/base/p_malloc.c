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
#include <stddef.h>
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

int p_malloc(int team, size_t size)
{

    printf("Running p_malloc(%d,%d)\n", team, (int)size);
    p_mem_t *mem;

    switch (0) {   // FIX!
    case EPIPHANY: // shared memory model
        break;
    case FPGA: // shared memory model
        break;
    case GPU: // shared memory model
        break;
    case SMP: // heap (thread model)
    /*
    mem=malloc(sizeof(p_mem_t));
    mem->mutex  = 23;
    mem->takeit = 0;
    mem->gotit  = 0;
    mem->size   = size;
    mem->memptr = malloc(size);
    break;
    */
    case GRID: // file+IP (rcp?)
        break;
    default:
        return (1);
    }
    return (0);
}
