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
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"
int p_open(int dev, int start, int count){
    printf("Running p_open(%d,%d,%d)\n",dev, start,count);
    
    int index=p_team_table_global.size;
    p_dev_t *devptr= p_dev_table_global.devptr[dev];
    p_team_t *team;
    team = (p_team_t *) malloc(sizeof(p_team_t));
    team->devptr  = p_dev_table_global.devptr[dev];
    team->size    = count;//setting size of team 
    team->teamptr = malloc(count*sizeof(unsigned int));
    team->statptr = malloc(count*sizeof(unsigned char));
    p_team_table_global.size = p_team_table_global.size + 1; 
    return(index);
}
