/**
 *
 * Cleans up/finalizes the run time for device 'dev'. Should be called before 
 * exiting the main program.
 *
 * @param dev  Pointer to object containing information about team. 
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_finalize (p_dev_t *dev){

    printf("Running p_finalize(p_team_t)\n");

    return(0);
}
