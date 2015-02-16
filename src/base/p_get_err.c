#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Check returned PAL reference for error condition.
 *
 * @param ref   PAL reference to error check.
 *
 * @return      Returns the error code (if any).
 *              0 indicates success.
 */
int p_get_err(const p_ref_t ref)
{
    return p_ref_get_err(ref);
}
