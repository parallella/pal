/**
 *
 * Create a team of processors based on the the total list of processors
 * defined within the device 'dev'.
 *
 * @param dev   Pointer to object containing device information
 *
 * @param start Index of first processor within 'dev' to include in team
 *
 * @param size  Total number of processors in team.
 *
 * @return      Returns a reference. Negative value indicates error.
 *
 */
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"
p_team_t p_open(p_dev_t dev, int start, int count)
{
#if 0
    printf("Running p_open(%ld,%d,%d)\n", dev, start, count);
    int index = p_team_table_global.size;
    p_dev_t *devptr = p_dev_table_global.devptr[dev];
    p_team_t *team;
    team = (p_team_t *)malloc(sizeof(p_team_t));
    team->devptr = p_dev_table_global.devptr[dev];
    team->size = count; // setting size of team
    team->teamptr = malloc(count * sizeof(unsigned int));
    team->statptr = malloc(count * sizeof(unsigned char));
    p_team_table_global.size = p_team_table_global.size + 1;
    return (index);
#endif
    return p_ref_err(ENOSYS);

}
