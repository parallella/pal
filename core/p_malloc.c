#include <stddef.h>
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

void *p_malloc (p_team_t *team, int n, size_t size){

    p_mem_t *mem;

    printf("size is %zu\n",size);
    //mem->dev    = team-->dev;
    mem->size   = size;
    mem->memptr = malloc(size);
    return(mem);    
}
