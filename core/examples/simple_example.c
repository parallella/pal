#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_core.h>
#include <unistd.h>

int main (int argc, char **argv){
    
    //Stack variables
    int opt;
    int type=3;//1=EP,2=GRID,3=SMP
    int nodes=16;//16
    char *elf="./hello_task.elf";
    char *func="main";
    int status;
      
    //Pointers to dynamic objects
    p_dev_t *dev0;       //device information
    p_program_t *prog0;  //program to execute
    p_team_t *team0;     //working team
    p_mem_t *mem0;       //memory object
      
    //Program flow
    dev0   = p_init(type, 0);               //initialize system
    prog0  = p_load(dev0, elf);             //load executable file into memory
    team0  = p_open(dev0, 0, nodes);       //Open a team (additive)
    status = p_run(prog0, team0, func, 0, NULL, ASYNC); //run program on team
    status = p_barrier(team0);              //set barrier on work team
    status = p_free(team0);                //free the resource)anything with po
    status = p_finalize(dev0);             //close down the device    

}
