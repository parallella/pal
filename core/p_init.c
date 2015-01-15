/*
 *
 * FUNCTION:
 *
 * Initalizes the loader run time system information based on the device type
 * and flag arguments provided.
 *
 * ARGUMENTS:
 *
 * type      EPIPHANY   : in O/S version, this comes from devices driver,
 *                        when runnning on epiphany, information would need
 *                        to come from somewhere else (not O/S)
 *   
 *           ZYNQ       : Driver provides device information, not much needed, 
 *                        static image
 *                        
 *           OPENCL     : Uses existing OpenCL driver to query
 *     
 *           SMP        : O/S provides information about # cores
 *
 *           CLOUD      : Job dispatcher "SLURM" like?
 *
 *                          
 * flags     tbd
 */
#include "pal_core.h"
#include "pal_core_private.h"

int p_init (int type, int flags, p_dev_t dev){

  
    /*PLACE CODE HERE*/   

    //1.get system information based on search and flag (type)
    //2.stuff information into structure p_dev_t
    //3.return pointer to p_dev_t
    //4.exit 

    return(0);

}
