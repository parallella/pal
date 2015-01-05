/*
 *
 * FUNCTION:
 * p_open(): This call creates a team of processors from of the total
 *           available workforce discovered during p_init()
 *            
 * ARGUMENTS:
 * dev    - pointer to program (in memory) to launch
 * nstart - id of first processor in team
 * ntotal - total number of processors in team
 * ncols  - columns of a block (only used for 2D mesh architectures) 
 *          (rows=ntotal/cols)
 *
 * RETURN VALUE:
 *
 * 
 */

pal_team_t p_open (pal_dev_t *dev,  //device descriptor
		   int nstart,      //id of first processor in team
		   int ntotal,      //total number of processors in team
		   int ncols,       //columns in team block (rows=ntotal/cols)
		   int flags        //launch flags
		   ){
    
    /*PLACE CODE HERE*/   
    
    //1. Create a team structure  
    //2. "inherit" information from "dev"
    //3. No reservations, error checking?
    //4.
}
