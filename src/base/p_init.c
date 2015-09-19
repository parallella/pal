#include <pal.h>

/**
 *
 * Initalizes the loader run time system information based on the device type
 * and flag arguments provided. All necesssary system information is aggregated
 * in the an object 'dev' for use by other pal functions. An example of the
 * minimum amount of information needed includes the number of processors in
 * the system and an address map/scheme.
 *
 * @param type  The type of worker device being used
 *
 *        EPIPHANY - An array of RISC processors with distributed shared memory
 *        FPGA     - A set of FPGA accelerators supported by a host processor
 *        SMP      - Multiple core shared memory processor system
 *        GPU      - GPUs supported by the OpenCL runtime
 *        GRID     - A distributed system of processors with IP addresses
 *                   Can fall back to the local host as well ("batch")
 *
 * @param flags Bitmask field indicating runtime options
 *
 * @return      Returns a reference to the device object.
 *              A negative value indicates error.
 */

#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

p_dev_t p_init(int type, int flags)
{
    struct dev *dev = NULL;

    if (type < P_DEV_FIRST || P_DEV_LAST < type)
        return p_ref_err(EINVAL);

    dev = __pal_global.devs[type];

    /* Check if compiled in */
    if (!dev)
        return p_ref_err(ENOSYS);

    return dev->dev_ops->init(dev, flags);
}
