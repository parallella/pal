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
    printf("Running p_load(%d,%s,%s,%d)\n",dev,file, function, flags);

    int index=p_program_table_global.size;
    p_dev_t *devptr= p_dev_table_global.devptr[dev];
    p_program_t *prog;

    //Creating a program structure
    prog = (p_program_t *) malloc(sizeof(p_program_t));    
    prog->devptr=devptr;
    prog->name=file;

    //Writing into global table
    p_program_table_global.progptr[index] = prog; 
    p_program_table_global.size = p_program_table_global.size + 1; 
  
    return(index);
}
