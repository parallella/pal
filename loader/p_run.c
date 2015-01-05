/*
 *
 * FUNCTION:
 * p_run(): Runs a program on a set of processors. Use cases:
 *          -run on all processors in set
 *          -run on one processor (any processor)
 *            
 * ARGUMENTS:
 * prog  - pointer to program (in memory) to launch
 * team  - team of processors to run on
 * nargs - number of program arguments
 * args  - array of pointers to program arguments
 * flags - run time flags
 *         ALL  - broadcasts to all members from processor 'np' to (N-1)
 *      
 * RETURN VALUE:
 * TIMEOUT_ERROR | SUCCESS
 * 
 */

void p_run (pal_program_t *prog, //program to run
	    pal_team_t *team,    //team of processors to run on
	    int np,              //processor offset within team (0..N-1),  
	    int nargs,           //number of program arguments
	    void** args,         //array of pointers to program arguments
	    int flags            //launch flags
   	   ){

    /*PLACE CODE HERE*/   

    //1.Verify that resource based on selected flag are available (how?), otherwise try again, eventually time out
    //2.broadcast/copy program to processors in team
    //3.broadcast/copy arguments to processors in team
    //4.start program (sync interrupt for epiphany)
    //5.exit 
}
