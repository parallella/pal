#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

void *p_load (p_dev_t *dev, char *file){
    printf("Running p_load(p_dev_t,%s)\n",file);

    p_program_t *prog;

    prog = (p_program_t *) malloc(sizeof(p_program_t));
    prog->dev=dev;
    prog->name=file;
    switch(prog->dev->property[TYPE]){	
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
        return(NULL);
    }	
    return(prog);
}
