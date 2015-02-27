#include <pal.h>

/**
 *
 * Remove members from an existing team of processors.
 *
 * @param team  Pointer to existing team object
 *
 * @param start Index of first processor from 'dev' (referenced by 'team')
 *              to remove from the existing team
 *
 * @param size  Total number of processors to remove. (following 'start')
 *
 * @return      Returns reference to team if successful. Negative value
 *              indicates error.
 *
 */

#include "pal_base.h"
#include "pal_base_private.h"

p_team_t p_remove(p_team_t team, int start, int size) { return (0); }
