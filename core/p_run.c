/**
 *
 * Runs(launches) the program 'prog' on all of the members of 'team'.
 *
 * @param prog      Pointer to the program to run that was loaded with p_load();
 *
 * @param team      Team to run with.
 *
 * @param start     Relative starting processor within team
 *
 * @param size      Total number of processors within team to run
 *
 * @param argn      Number of arguments to be supplied to 'function'
 *
 * @param args      An array of pointers to function arguments
 *
 * @param flags     Bitfield mask type flags 
 *       
 * @return          Returns 0 if successful.
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_run(int prog, int team, int start, int size,
	  int argn, void *args[], int flags){
    
    printf("Running p_run(%d,%d,%d,%d,%d, argv,%d)\n", prog, team, start,size,argn,flags);


    pid_t child_pid[16],wpid,i;
    int status=0;	
    
    switch(0){//FIX!! (implement with FD)
    case EPIPHANY:
	break;
    case FPGA:
        break;  
    case GPU:
        break;  
    case SMP:		
	//NOTE!: fork or join?, shared memory objects much more efficient 
	//with threads
	printf("Forking %d processes!\n",size);
	//char * const elf[]={prog->name, NULL};
	//char * path=prog->name;	 
	 for(i=0;i<size;i++){
	     child_pid[i]=fork();
	     if (child_pid[i] == 0) {
		 //HACK: this is tricky, leaving this for the professionals..
		 //how to run a program based on function name?
		 //my_average(input,n,output);
		 exit;
	     } 
	 }	 	 
	 //Waiting for all children to finish. Right way?
	 for(i=0;i<size;i++){
	     while(0<waitpid(child_pid[i],NULL,0));
	 }
	 break;
    case GRID:
        break;	
    default:
        return(1);
    }
         
}
/*
void my_average (float *a, int size, float *res){
    
    int i;
    *res=0;
    printf("hello\n");
    for(i=0;i<size;i++){
	(*res)+=*(a+i);	
    }
    *res=(*res)/(float) size;
    printf("--Average of this batch is %f--\n", *res);
}
*/
