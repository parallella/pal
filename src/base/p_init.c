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
 * @return      Returns a pointer to the device object
 *              Returns NULL on error
 */

#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

int p_init(int type, int flags)
{
    int index = p_dev_table_global.size;
    p_dev_t *dev;

    printf("Running p_init(%d,%d)\n", type, flags);
    // Store information about system somewhere...
    switch (type) {
    case EPIPHANY:
        if (flags & LINUX) {
            /*Get information from kernel driver/device tree*/
            /*store information in structure p_dev_t */
            dev = (p_dev_t *)malloc(sizeof(p_dev_t));
            /*putting in static values for now...magic later!*/
            dev->property[TYPE] = type;          /*storing type of structure*/
            dev->property[VERSION] = 0xDEADBEEF; /*not needed in ideal case*/
            dev->property[NODES] = 16;
            dev->property[TOPOLOGY] = 2; /*2D mesh*/
            dev->property[ROWS] = 4;
            dev->property[COLS] = 4;
            dev->property[PLANES] = 4;
            dev->property[CHIPROWS] = 4;
            dev->property[CHIPCOLS] = 4;
            dev->property[SIMD] = 1;        /*epiphany is a scalar processor*/
            dev->property[MEMSIZE] = 32768; /*32KB*/
            dev->property[MEMBASE] = 0x80800000; /*array origin*/
        } else if (flags & METAL) {
            /*Get information from a static C-run time structure*/
        }
        break;
    case FPGA:
        /*same style as epiphany*/
        break;
    case GPU:
        /*assume an installed OpenCL env*/
        /*clinfo*/
        break;
    case SMP:
        /*query the hardware for resources*/
        /*stuff into structure*/
        dev = (p_dev_t *)malloc(sizeof(p_dev_t));
        dev->property[TYPE] = type;          /*storing type of structure*/
        dev->property[VERSION] = 0xDEADBEEF; /*not needed in ideal case*/
        dev->property[NODES] = 4;
        dev->property[SIMD] = 4;
        dev->property[MEMSIZE] = 32768;      /*32KB*/
        dev->property[MEMBASE] = 0x80800000; /*array origin*/
        break;
    case GRID:
        /*pass through for slurm?*/
        /*create batch file*/
        /*.. srun -n 8 ./hello.elf*/
        break;
    case DEMO:
        dev = (p_dev_t *)malloc(sizeof(p_dev_t));
        dev->property[TYPE] = type;
        dev->property[NODES] = 4;
        p_dev_table_global.devptr[index] = dev;
        p_dev_table_global.size = p_dev_table_global.size + 1;
        break;
    default:
        return (ERROR);
    }
    return (index);
}
