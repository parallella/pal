/**
 *
 * Add members to an existing team of processors.
 *
 * @param team  Team number
 *
 * @param start Index of first processor from 'dev' (referenced by 'team')
 *              to be added to the existing team
 *
 * @param size  Total number of processors to add.(following 'start')
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_append(int team, int start, int size)
{
    printf("Running p_append(p_team_t,%d,%d)\n", start, size);
    // need to implement
    return (0);
}
