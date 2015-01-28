/**
 *
 * Cleans up/finalizes the run time for device 'dev'. Should be called before 
 * exiting the main program.
 *
 * @param dev   Device object desriptor (int)
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_finalize (int dev){
    printf("Running p_finalize(%d)\n", dev);
    return(0);
}
