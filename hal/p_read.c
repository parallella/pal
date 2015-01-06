/*
 *
 * FUNCTION:
 * p_read():   reads data from global space to a local variable
 *
 * ARGUMENTS:
 * loc_dst    - local variable destination pointer
 * src        - complete address source pointer
 * nb         - number of bytes to copy
 * flags      - independent flags to assert, separate with '|'
 *              ASYNC : Makes copy call asynchronous (ie non-blocking)  
 *         
 */
#include <stddef.h>
void *p_read (void *loc_dst, const void *src, size_t nb, int flags){

    /*PLACE CODE HERE*/

}
