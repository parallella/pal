#include <pal_core.h>
#define N 16 
int main (int argc, char **argv){    
    //Stack variables
    char *file="./average.elf";
    char *func="main";
    int status, i, all, nargs=1;   
    void* args[1];
    float a[N] = {1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,
		  9.0,10.0,11.0,12.0,13.0,14.0,16.0};

    //Integer index into opaque structures
    int dev0, prog0, team0, mem[4];

    //Execution setup
    dev0   = p_init(SMP, 0);                  //Initialize device and team
    all    = p_query(dev0, NODES);            //Find number of nodes in system
    team0  = p_open(dev0, 0, all);            //Create a team
    prog0  = p_load(dev0, file, func, 0);     //Load a program from file system    

    //Allocate memory
    for(i=0;i<all;i++){
	mem[i] = p_malloc(team0, N/4*sizeof(float));
    }

    //Copy stack data to device memory
    for(i=0;i<all;i++){
	p_write(mem[i], &a[i*4], N/4*sizeof(float), 0);	
    }        
    //Run the program
    for(i=0;i<all;i++){
	args[0]= &mem[i];
	status = p_run(prog0, team0, i, 1, nargs, args, ASYNC);
    }            
    p_barrier(team0);    //Wait until all are done
    
    for(i=0;i<all;i++){
	p_free(mem[i]);  //Free memory objects
    }       
    p_close(team0);      //Close team   
    p_finalize(dev0);    //Finalize memory
}


