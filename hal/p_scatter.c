/*
 *
 * FUNCTION:
 * p_scatter(): scatters an array of bytes to list of destination
 *              addresses
 * ARGUMENTS:
 * dst_list - complete address destination pointer
 * loc_src  - local source pointer
 * nb       - number of bytes to copy
 * nd       - number of destinations
 * flags    - independent flags to assert, separate with '|'
 *            ASYNC : Makes copy call asynchronous (ie non-blocking)  
 *
 */
#include <stddef.h>
void *p_scatter (void *dst_list, const void *loc_src, size_t nb, int nd, int flags){

    /*PLACE CODE HERE*/

}
