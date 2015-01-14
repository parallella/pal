#include <stdio.h>
#include <stdlib.h>
#include <pal_core.h>
#define N 32

int main(int argc, char *argv[]){
    
    //Some variable declarations
    size_t bufsize = N * sizeof(float);   

    //Initialize system
    pal_dev_t dev0 =  pal_init(EPIPHANY, 0);

    //Query system for information
    int *all   = pal_query(dev0, ALL);    //list of all all processor ids
    int *myid  = pal_query(dev0, WHOAMI); //this processor
        
    //Load an ELF file from the file system
    pal_program_t prog0 = pal_load(dev0, "./my.elf");

    //Create a working team
    pal_team_t team0 = pal_open(dev0, all, 0);
    
    //Allocate regular memory
    float* a = (float*) malloc(bufsize);
    float* b = (float*) malloc(bufsize);
    float* c = (float*) malloc(bufsize);

    //Allocate working device memory (per processor)
    pal_mem_t mem0 = pal_malloc(team0, *myid, bufsize);    
    pal_mem_t mem1 = pal_malloc(team0, *myid, bufsize);    
    pal_mem_t mem2 = pal_malloc(team0, *myid, bufsize);
    
    //Copy a working set to shared memory (event ignored)
    pal_write (a, mem0, bufsize, 0);
    pal_write (b, mem1, bufsize, 0);

    //Pass the arguments to the program

    /*fix later
      int argc=3;
      int argv;
      char argv[0] = pal_getaddr(mem0);
      char argv[1] = pal_getaddr(mem1);
      char argv[2] = pal_getaddr(mem2);
    */
    //Run the program on a team of processors
    pal_run(team0, prog0, argc, argv, 0);

    //Read back data from shared memory 
    pal_read(mem2, c, bufsize, 0);

    //Close down the team
    pal_close(team0);

    //Free up device memory 
    pal_free(mem0);
    pal_free(mem1);
    pal_free(mem2);

    //Free up regular malloc memory
    free(a);
    free(b);
    free(c);

    //Close down device connection
    pal_finalize(dev0);    
}
