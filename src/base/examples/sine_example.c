#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_base.h>
#include <unistd.h>

#define N 1e6

int main(int argc, char **argv)
{

    // Stack variables
    int type = 3; // 1=EP,2=GRID,3=SMP
    char *elf = "./sine_task.elf";
    char *func = "main";
    int status, myid, i, all;

    // Pointers to dynamic objects
    p_dev_t *dev0;      // device information
    p_program_t *prog0; // program to execute
    p_team_t *team0;    // working team
    p_mem_t *mem0;      // memory object (input)
    p_mem_t *mem1;      // memory object (output)

    // Setting arguments

    // Initialize memory/team
    dev0 = p_init(type, 0);       // initialize system
    all = p_query(dev0, NODES);   // find # of device nodes
    prog0 = p_load(dev0, elf);    // load executable file
    team0 = p_open(dev0, 0, all); // open a team

    // Allocate memory
    mem0 = p_malloc(team0, N * sizeof(float)); // allocate local memory object
    mem1 = p_malloc(team0, N * sizeof(float)); // allocate local memory object

    // Setting arguments
    unsigned int nargs = 1;
    void *args[] = {&mem0};

    // Running function
    status = p_run(prog0, team0, func, nargs, args, ASYNC);

    // Wait until all team members have finished
    status = p_barrier(team0);

    // Cleanup
    status = p_free(mem0);     // free memory
    status = p_close(team0);   // close the team
    status = p_finalize(dev0); // close down the device
}
