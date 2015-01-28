/**
 *
 * Loads a program from a file into an object in memory and prepares the program
 * for execution.
 *
 * @param dev       Pointer to object containing device information 
 *
 * @param file      File name of executable to load.
 *
 * @param function  Name of function within 'prog' to run
 *
 * @return          Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"
int p_load (int dev, char *file, char *function, int flags){
    int index,type;

    printf("Running p_load(%d,%s)\n",dev,file);

    //Find device type
    type=p_query(dev,TYPE);

    switch(type){	
    case EPIPHANY:
	break;
    case FPGA:
        break;  
    case GPU:
        break;  
    case SMP:	
        break;
    case GRID:
        break;
    default:
        return(ERROR);
    }	
    return(index);
}
