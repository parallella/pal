#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_core.h>

int main(int argc, char *argv[]){
      
    p_dev_t *dev0;       //device information object
    p_program_t *prog0;  //in memory exetutable object
    p_team_t *team0;     //working team object
    p_mem_t *mem0;       //memory object

    int status;
    int nodes;

    dev0   = p_init(EPIPHANY, 0);           //initialize system
    prog0  = p_load(dev0, "./hello.elf");   //load executable file into memory
    nodes  = p_query(dev0, NODES);          //query how many nodes are in system
    team0  = p_open(dev0, 0, nodes);        //Open a team (additive)
    status = p_run(prog0, team0, 0, NULL, ASYNC); //run program on team
    status = p_barrier(team0);              //set barrier on work team
    status = p_free(team0);                //free the resource)anything with po
    status = p_finalize(dev0);             //close down the device    
}
