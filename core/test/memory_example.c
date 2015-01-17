#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <pal_core.h>
#define N 8
int main(int argc, char *argv[]){
      
    p_dev_t *dev0;       //device information object
    p_program_t *prog0;  //in memory exetutable object
    p_team_t *team0;     //working team object
    p_mem_t *mem0;       //memory object

    int status;
    int n;
    size_t size=N*sizeof(int);
    int a[N]={1,2,3,4,5,6,7,8};
    int c[N];
    int i;

    dev0   = p_init(EPIPHANY, 0);      //initialize system
    n      = p_query(dev0, NODES);     //query #nodes in system    
    team0  = p_open(dev0, 0, n);       //open a team of "n" nodes
    mem0   = p_malloc(team0,0,size);   //allocate memory on processor 0
    status = p_write(&a,size,0,mem0);  //copying from a to mem0
    status = p_read(mem0,size,0,&c);   //copying from mem0 to c  
    status = p_finalize(dev0);         //close down the device    
    for(i=0;i<N;i++){
	printf("c[%d]=%d\n",i,c[i]);
    }
}
