/*
 *
 * FUNCTION:
 * p_open(): This call creates a team of processors from of the total
 *           available workforce discovered during p_init()
 *            
 * ARGUMENTS:
 * dev    - device descriptor
 * list   - a list of processor ids from *dev
 * flags  - 
 *
 * RETURN VALUE:
 *
 * 
 */

pal_team_t p_open (pal_dev_t *dev,  //device descriptor
		   pal_pid_t *list, //a list of processor ids from *dev
		   int flags        //launch flags
		   ){
    
    /*PLACE CODE HERE*/   
    
    //1. Create a team structure  
    //2. "inherit" information from "dev"
    //3. No reservations, error checking?
    //4.

    //Place the barrier initialization in here as well.
    //1.Look at team structure 
    //2.Find out the list of processors in the 
    //3.Initialize the counter in teh team structure to N (equal to 
    //  number of processors in team)

}
