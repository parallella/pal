#include <pal.h>

#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

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

int p_finalize(p_dev_t dev)
{
    struct dev *pdev = (struct dev *) dev;
    printf("Running p_finalize(%p)\n", dev);

    if (p_ref_is_err(dev))
        return -EINVAL;

    pdev->dev_ops->fini(dev);

    return 0;
}
