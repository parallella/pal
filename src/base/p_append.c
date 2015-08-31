#include <pal.h>

/**
 *
 * Add members to an existing team of processors.
 *
 * @todo need to implement
 *
 * @param team  Team number
 *
 * @param start Index of first processor from 'dev' (referenced by 'team')
 *              to be added to the existing team
 *
 * @param size  Total number of processors to add.(following 'start')
 *
 * @return      Returns reference to team if successfull. Negative number
 *              indicates error.
 */
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

p_team_t p_append(p_team_t team, int start, int size)
{
    UNUSED(team);
    UNUSED(start);
    UNUSED(size);

    printf("Running p_append(p_team_t,%d,%d)\n", start, size);
    // need to implement
    return (0);
}
