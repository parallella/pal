#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"
int p_run(p_program_t *prog, p_team_t *team, int argn, void **args, int flags){

    int i, id;
        
    for(i=0;i<(team->size);i++){
	printf("Running program on core %d\n",i);
	//launch program to node "id"
	//need to extend to treat an arbitrary size list of IDs
	//don't assume contiugous arrays! 
    }
    return(0);
}
