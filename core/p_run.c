#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_run(p_program_t *prog, p_team_t *team, char *function, 
	  int argn, void **args, int flags){
    
    printf("Running p_run(p_dev_t,p_team_t, %s, %d, argv,%d)\n",function,argn,flags);
    

    int i;
    pid_t child_pid[16],wpid;
    int status=0;	

    switch(team->dev->property[TYPE]){	
    case EPIPHANY:
	break;
    case FPGA:
        break;  
    case GPU:
        break;  
    case SMP:		
	printf("Forking %d processes!\n",team->size);
	 char * const elf[]={prog->name, NULL};
	 char * path=prog->name;	 
	 for(i=0;i<(team->size);i++){
	     child_pid[i]=fork();
	     if (child_pid[i] == 0) {
		 execve(path,elf,NULL);
		 exit;
	     } 
	 }	 	 
	 //Waiting for all children to finish. Right way?
	 for(i=0;i<(team->size);i++){
	     while(0<waitpid(child_pid[i],NULL,0));
	 }
	 break;
    case GRID:
        break;
    default:
        return(1);
    }
         
}
