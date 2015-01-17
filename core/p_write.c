#include <stddef.h>
#include <string.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_write (void *src, size_t nb, int flags, p_mem_t *mem){
        
    memcpy(mem->memptr,src,nb);
    return(0);
}
