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

    /*
      p_team_t *team;
      team = (p_team_t *) malloc(sizeof(p_team_t));
      team->dev     = dev; //setting pointer to device structure
      team->size    = size;//setting size of team 
      team->teamptr = malloc(size*sizeof(unsigned int));
      team->statptr = malloc(size*sizeof(unsigned char));
    */
    return(0);
}
