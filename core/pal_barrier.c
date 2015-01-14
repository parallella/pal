/*
 *
 * FUNCTION:
 * pal_barrier(): Wait until all members in a team have reached this point
 *            
 * ARGUMENTS:
 * team
 *
 * RETURN VALUE:
 *
 * 
 */
#include "pal_core.h"
#include "pal_private.h"

void pal_barrier (pal_team_t *team){

    /*PUT CODE HERE*/
    //1.Every processor on the team runs this function
    //2.When it hits, decrement the counter location in the "boss node"
    //3.(decrement needs to be an atomic fetch and subtract)
    //(alternatively, there could be one entry for each core)
    //4.team members "idle" and wait for signal from boss
    //5.after completion, the boss clears table (resets counter)
    //5.boss processer sends signal back continue signal 

    //assumptions:
    //1.) the workers have a team structures indicating the upstream root
    //structure location
    //2.) this structure gets pushed out durung the p_open call
    //3.) 

}
