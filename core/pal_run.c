/*
 *
 * FUNCTION:
 * p_run(): Runs a program on a set of processors. Use cases:
 *          -run on all processors in set
 *          -run on one processor (any processor)
 *            
 * ARGUMENTS:
 * team  - team of processors to run on
 * prog  - pointer to program (in memory) to launch
 * argc  - "main" style
 * argv  - "main" style
 * flags - run time flags
 *         ALL  - broadcasts to all members from processor 'np' to (N-1)
 *      
 * RETURN VALUE:
 * TIMEOUT_ERROR | SUCCESS
 * 
 */
#include "pal_core.h"
#include "pal_core_private.h"

void *pal_run (pal_team_t team,    //team of processors to run on
	       pal_program_t prog, //program to run
	       int argc,            //# arguments
	       char *argv[],        //array of character strings
	       int flags            //launch flags
	       ){

    void * result;

    /*PLACE CODE HERE*/   

    //1.Verify that resource based on selected flag are available (how?), otherwise try again, eventually time out
    //2.broadcast/copy program to processors in team
    //3.broadcast/copy arguments to processors in team
    //4.start program (sync interrupt for epiphany)
    //5.exit 

    return(result);
}
