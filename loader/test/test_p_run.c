#include <pal.h>
void main(){

    int i;

    //Note! Just use case snippets for now, not functional

    //one processor at a time within team
    for(i=0;i<16;i++){
	p_run(prog,my_team,i,0,NULL);
    }
    
    //broadcast to all members
    p_run(prog,my_team,0,0,NULL,RUN_ALL);
}
