/**
 *
 * Copies the content of 'mem' into the area pointed to by the 'dst' pointer.
 * 
 *
 * @param mem   Pointer to a memory object created with p_malloc();
 *
 * @param nb    Number of bytes to copy
 * 
 * @param flags Bitmask field indicating runtime options
 *              
 *        ASYNC: Specifies asynchronous (non-blocking) operation. 
 *               The default is blocking    
 *
 * @param dst   Address to copy the memory block to
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_read (p_mem_t *mem, size_t nb, int flags, void *dst){
    printf("Running p_read(p_mem_t,%d,%d,%p)\n", (int) nb,flags, dst);
    memcpy(dst, mem->memptr,nb);
    return(0);
}
