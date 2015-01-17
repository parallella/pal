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
 *        OPENCL   - Platforms supporting the OpenCL runtime
 *        GRID     - A distributed system across a network
 *
 * @param flags Bitmask field indicating runtime options
 *       
 * @return      Returns a pointer to the device object 
 *              Returns NULL on error
 */

#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"
void *p_init (int type, int flags){
    
    p_dev_t *dev;

    switch (type) {	
    case EPIPHANY:	
	if(flags & LINUX){
	    /*Get information from kernel driver/device tree*/	    
	}
	else if(flags & METAL){
	    /*Get information from a static C-run time structure*/ 
	    /*Is this kind of portability possible?*/
	}
	else{
	    dev=(p_dev_t *) malloc(sizeof(p_dev_t));
	    /*putting in static values for now...magic later!*/ 	    
	    dev->property[TYPE]=type;/*storing type of structure*/
	    dev->property[VERSION]=0xDEADBEEF;/*not needed in ideal case*/
	    dev->property[NODES]=16;
	    dev->property[TOPOLOGY]=2;/*2D mesh*/	    
	    dev->property[ROWS]=4;
	    dev->property[COLS]=4;
	    dev->property[PLANES]=4;
	    dev->property[CHIPROWS]=4;
	    dev->property[CHIPCOLS]=4;
	    dev->property[SIMD]=1;/*epiphany is a scalar processor*/
	    dev->property[MEMSIZE]=32768;/*32KB*/
	    dev->property[MEMBASE]=0x80800000;/*array origin*/
	}
	break;	
    case FPGA:
	break;	
    case OPENCL:
	break;	
    case SMP:
	break;
    case GRID:
	break;
    default:
	return(NULL);
    }
    printf("Finished p_init\n");
    return(dev);
}
