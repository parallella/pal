#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_base.h>
#include <unistd.h>

#define N 8

int main(int argc, char **argv)
{

    // Pointers to opaque PAL objects
    p_dev_t *dev0;      // device information
    p_program_t *prog0; // program to execute
    p_team_t *team0;    // working team
    p_mem_t *mem0;      // memory object

    // Stack variables
    int type = 3; // 1=EP,2=GRID,3=SMP
    int status, myid, i, all;
    int a[N] = {1, 2, 3, 4, 5, 6, 7, 8};
    int b[N] = {0, 0, 0, 0, 0, 0, 0, 0};

    // PAL flow
    dev0 = p_init(type, 0);                  // initialize system
    myid = p_query(dev0, WHOAMI);            // find my id
    all = p_query(dev0, NODES);              // find # of device nodes
    team0 = p_open(dev0, 0, all);            // open a team
    mem0 = p_malloc(team0, N * sizeof(int)); // allocate mem0 memory object
    status = p_write(a, N * sizeof(int), 0, mem0); // write data to mem0
    status = p_read(mem0, N * sizeof(int), 0, b);  // read data from mem0
    status = p_free(mem0);                         // free memory
    status = p_close(team0);                       // close down team
    status = p_finalize(dev0);                     // close down the device

    // Checking results
    for (i = 0; i < N; i++) {
        printf("b[%d]=%d\n", i, b[i]);
    }
}
