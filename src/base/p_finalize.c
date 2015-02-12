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
#include "pal_base.h"
#include "pal_base_private.h"

int p_finalize(p_dev_t dev)
{
    printf("Running p_finalize(%ld)\n", dev);
    return (0);
}
