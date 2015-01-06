/*
 *
 * FUNCTION:
 * p_finalize(): Closes the device connection.
 *               If this is the last process to use that device, then
 *               the underlying driver takes care of shutting down the device.
 *               In an O/S env, the process is automaitcally killed at the
 *               exit of the program.   
 *
 * ARGUMENTS: 
 *
 * pal_dev_t : Device conncetion to finalize
 *                          
 * RETURN VALUE:
 *
 */

p_dev_t p_finalize (pal_dev_t *dev){

    /*PLACE CODE HERE*/   

    //1.decrements some kind of "usage" semaphore in driver
    //2.exit 
}
