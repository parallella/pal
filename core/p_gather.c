/*
 *
 * FUNCTION:
 * p_gather(): gathers arrays of bytes from a set of sources into
 *             a local array
 * ARGUMENTS:
 * loc_dst  - complete address destination pointer
 * src_list - complete address source pointer
 * nb       - number of bytes to copy
 * ns       - number of sources to copy from
 * flags    - independent flags to assert, separate with '|'
 *            ASYNC : Makes copy call asynchronous (ie non-blocking)  
 *         
 */
#include <stddef.h>
#include "pal_core.h"
#include "pal_private.h"

int p_gather (void** srclist, size_t nsrc, size_t ndst, int flags, void *dst){

    /*PLACE CODE HERE*/

}
