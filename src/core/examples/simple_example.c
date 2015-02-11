#include "pal_core.h"
#include <stdio.h>
#define N 16
int main(int argc, char *argv[])
{

    // Stack variables
    char *file = "./hello_task.elf";
    char *func = "main";
    int status, i, all, nargs = 1;
    void *args[nargs];
    char argbuf[20];

    // Integer index into opaque structures
    int dev0, prog0, team0, mem[4];

    // Execution setup
    dev0 = p_init(DEMO, 0);              // initialize device and team
    prog0 = p_load(dev0, file, func, 0); // load a program from file system
    all = p_query(dev0, NODES);          // find number of nodes in system
    team0 = p_open(dev0, 0, all);        // create a team

    // Running program
    for (i = 0; i < all; i++) {
        sprintf(argbuf, "%d", i); // string args needed to run main asis
        args[0] = &argbuf;
        status = p_run(prog0, team0, i, 1, nargs, args, 0);
    }
    p_barrier(team0); // not needed
    p_close(team0);   // close team
    p_finalize(dev0); // finalize memory
}
