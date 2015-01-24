/**
 *
 * Dynamically allocates contiguous memory buffers at any node within
 * the specified 'team' object. All inter-node data communication within a 
 * team is done between these dynamically allocated memory blocks. The function 
 * returns a pointer to the buffer of allocated memory. 
 *
 * @param team  A pointer to the object defining the team structure.
 *              The team contains a set of compute nodes with each node having
 *              a finite amount of memory.
 *
 * @param n     The relative node within the 'team' starting at 0.
 *       
 * @return      Returns a pointer to the memory buffer 
 *              Returns NULL on error
 */
#include <stddef.h>
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

void *p_malloc (p_team_t *team, int n, size_t size){
    printf("Running p_malloc(p_team_t,%d,%d)\n", n, (int) size);
    p_mem_t *mem;
    mem=malloc(sizeof(p_mem_t));        
    mem->size   = size;
    mem->memptr = malloc(size);
    return(mem);    
}
