/**
 *
 * Frees up resources occupied by 'obj'. The object can be of type, 
 * p_dev_t, p_mem_t, or p_team_t.
 *
 * @param obj Pointer to object to free.
 *
 * @return    Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_free (void *obj){
    printf("Running p_free(obj)\n");
    return(0);
}
