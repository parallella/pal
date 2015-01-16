#include <stdio.h>
#include <stdlib.h>
#include <pal_core.h>

int N 32
int main(int argc, char *argv[]){

    //Opaque objects	   
    p_dev_t dev0;        //device structure
    p_program_t prog0;   //program structure
    p_team_t team0;      //team structure
    p_team_t mem0;       //memory structure
    int num_cpu=0;
    int a[N];
    int b[N];
    size_t bufsize N*sizeof(int);

    p_init(EPIPHANY, DEFAULT, dev0);        //initialize system
    p_open(dev0, num_cpu, 1, team0);        //assign a member to team    
    p_malloc(team0, num_cpu, bufsize, mem0);//allocating a memory buffer
    p_write(&a,bufsize,BLOCK,mem0);         //copy buffer from 'a' to mem object
    p_read(mem0,bufsize,BLOCK,&b);          //copy buffer from mem object to 'b'
    p_close(team0);
    p_finalize(dev0);
}
