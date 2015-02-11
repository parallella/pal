/*
 *
 * FUNCTION:
 * p_copy(): copies an array of bytes from source to destination
 *
 * ARGUMENTS:
 * dst   - complete address destination pointer
 * src   - complete address source pointer
 * nb    - number of bytes to copy
 * flags - independent flags to assert, separate with '|'
 *         ASYNC : Makes copy call asynchronous (ie non-blocking)
 *
 * NOTES:
 * 0. User space
 * 1. Specific to bsb/device/os
 * 2. Should be lightning fast
 * 3. Add "safety" compile switch for error checking
 * 4. Function should not call any
 * 5. Need a different call per chip, board, O/S?
 *
 */
#include <stddef.h>
#include "pal_base.h"
#include "pal_private.h"

ssize_t p_memcpy(void *dst, void *src, size_t nb, int flags)
{
    /*PLACE CODE HERE*/
    return (0);
}
