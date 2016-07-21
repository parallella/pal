#include <pal.h>

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Map device memory. Not all devices can implement this.
 * It is okay to return NULL.
 *
 * @param dev       Device
 * @param address   Device address
 * @param size      Size
 *
 * @return          Raw pointer or NULL on error or not supported.
 *
 */
void *_p_map_raw(p_dev_t dev, unsigned long addr, unsigned long size)
{
    struct dev *pdev= (struct dev *) dev;

    if (p_ref_is_err(dev))
        return NULL;

    if (!pdev || !pdev->dev_ops || !pdev->dev_ops->_map_raw)
        return NULL;

    return pdev->dev_ops->_map_raw(pdev, addr, size);
}
