/**
 *
 * A memory fence. Ensures that all read/write operations to memory object
 * from the processor exeucting the p_fence call have completed. 
 *
 * @param mem   Pointer to a memory object
 *
 * @return      Returns 0 if successful.
 *
 */
#include "pal_core.h"
#include "pal_private.h"

int p_fence (p_mem_t *mem){

    return(0);
    
}
