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
    int status;
    int myid;
  
    //Pointers to dynamic objects
    p_dev_t *dev0;       //device information
    p_program_t *prog0;  //program to execute
    p_team_t *team0;     //working team
    p_mem_t *mem0;       //memory object
          
    //Program flow
    dev0   = p_init(type, 0);              //initialize system
    team0  = p_open(dev0, 0, nodes);       //Open a team (additive)
    mem0   = p_malloc(team0,0,0);          //allocate mem0 memory object
    p_write(inptr,N,0,mem0);               //write data to mem0
    p_read(mem0,N,0,outptr);               //read data from mem0
    status = p_free(mem0);                 //free mem
    status = p_close(team);                //close down team
    status = p_finalize(dev0);             //close down the device    

}
