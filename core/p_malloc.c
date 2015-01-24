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
#include "pal_core.h"
#include "pal_core_private.h"

void *p_malloc (p_team_t *team, size_t size){
    printf("Running p_malloc(p_team_t,%d)\n", (int) size);
    p_mem_t *mem;
    mem=malloc(sizeof(p_mem_t));        
    mem->size   = size;
    mem->memptr = malloc(size);
    return(mem);    
}
