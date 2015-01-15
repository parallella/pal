#include <stdio.h>
#include <stdlib.h>
#include <pal_core.h>

int main(int argc, char *argv[]){

    //Opaque objects	   
    p_dev_t dev0; 
    p_program_t prog0;
    p_team_t team0;
    
    //Local variables
    int total;   

    //Initialize system
    p_init(EPIPHANY, DEFAULT, dev0);
   
    //Load an ELF file from the file system
    p_load(dev0, "./hello.elf", prog0);

    //Dynamically create a team
    p_query(dev0, TOTAL, &total); 

    //1.Create a bit mask one bit per processor(0..N)
    //2.This should be dynamic depending information from query, regular malloc
    //3.For embedded system, it could be fixed(N)
    
    //Open a team (additive)
    p_open(dev0, 0, total, team0);
    
    //Run the program "process" on a team of processors
    int nargs = 0;
    void*  args[]={};	      	
    p_run(team0, prog0, nargs, args, ASYNC);

    //Wait for completion     
    p_wait(team0);

    //Close down the team
    p_close(team0);

    //Close down device
    p_finalize(dev0);    
}
