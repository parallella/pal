#include <pal.h>
#include <stdio.h>
#include <string.h>

#define N 16
int main(int argc, char *argv[])
{

    // Stack variables
    const char *file = "./hello_task.elf";
    const char *func = "main";
    int status, i, all, nargs = 1;
    p_arg_t args[nargs];
    char argbuf[20];


    // References as opaque structures
    p_dev_t dev0;
    p_prog_t prog0;
    p_team_t team0;
    p_mem_t mem[4];

    // Execution setup
    dev0 = p_init(P_DEV_DEMO, 0);        // initialize device and team
    prog0 = p_load(dev0, file, 0); // load a program from file system
    all = p_query(dev0, P_PROP_NODES);   // find number of nodes in system
    team0 = p_open(dev0, 0, all);        // create a team

    // Running program
    for (i = 0; i < all; i++) {
        sprintf(argbuf, "%d", i); // string args needed to run main asis
        args[0].ptr = argbuf;
        args[0].size = strnlen(argbuf, 10);
        args[0].is_primitive = false;
        status = p_run(prog0, func, team0, i, 1, nargs, args, 0);
    }
    p_wait(team0);    // not needed
    p_close(team0);   // close team
    p_finalize(dev0); // finalize memory

    return 0;
}
