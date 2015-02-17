#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int pid = 0;
    int i;
    pid = atoi(argv[2]);
    for (i = 0; i < 1000000; i++) {
    }
    printf("--Processor %d says hello!--\n", pid);
    return i;
}
