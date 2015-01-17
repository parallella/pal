#include "pal_core.h"
#include "pal_core_private.h"

void *p_open (p_dev_t *dev, int start, int size){
    
    p_team_t *team;

    team=(p_team_t *) malloc(sizeof(p_team_t));
    team->dev   = dev;  //setting pointer to device structure
    team->size  = size;//setting size of team 

    //An array of team members, every member has:
    //1.a processor ID (integer)
    //2.a busy flag (gets set on launched, cleard on program completion)
    //3.Or the slow solution is to query the remote machines... 
    //  grid-->look to see if process exited on all machines
    //  epiphany-->poll the idle state
    //  smp-->see if process exited on all machines
    //4. what else?

    return(team);
}
