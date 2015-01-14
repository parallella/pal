/*
 *
 * FUNCTION:
 * pal_malloc(): Reserves memory as specified by size and returns 
 *               a pointer to the allocated buffer
 *
 * ARGUMENTS:
 * dev      - device structure "describes universe" 
 * n        - processor number
 * size     - number of bytes to allocate

 * RETURN VALUE:
 * Return a pointer to the allocated memory.
 * On error, the function returns NULL
 *
 */
#include <stddef.h>
#include "pal_core.h"
#include "pal_core_private.h"

void *pal_malloc (pal_team_t team, int n, size_t size){

    void *result;
    
    /*PLACE CODE HERE*/
    
    return(result);
    
}
