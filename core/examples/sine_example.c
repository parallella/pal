#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_core.h>
#include <unistd.h>

#define N 1e6

int main (int argc, char **argv){
    
    //Stack variables
    int type = 3;//1=EP,2=GRID,3=SMP  
    char *elf = "./sine_task.elf";
    char *func = "main";
    int status, myid, i, all;

    //Pointers to dynamic objects
    p_dev_t *dev0;       //device information
    p_program_t *prog0;  //program to execute
    p_team_t *team0;     //working team
    p_mem_t *mem0;       //memory object
    float *a,*b;         //input and output arrays
   

    //Allocating local memory
    a = malloc(N*sizeof(float));
    b = malloc(N*sizeof(float));
    
    //Setting arguments
      
    //Program flow
    dev0   = p_init(type, 0);               //initialize system
    myid   = p_query(dev0,WHOAMI);          //find my id
    all    = p_query(dev0, NODES);          //find # of device nodes
    prog0  = p_load(dev0, elf);             //load executable file into memory    
    team0  = p_open(dev0, 0, nodes);        //Open a team (additive)
    mem0   = p_gmalloc(team0,myid,0);       //allocate global memory object    
    status = p_run(prog0, team0, func, 0, NULL, ASYNC); //run program on team
    status = p_barrier(team0);              //set barrier on work team
    status = p_free(team0);                //free the resource)anything with po
    status = p_finalize(dev0);             //close down the device    

}
