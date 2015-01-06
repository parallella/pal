/*
 *
 * FUNCTION:
 * p_init(int flag): Initalizes the loader run time
 *           system information should be read in magically by driver layer
 *           in linux this would come from the device tree
 *   
 *           The function should do a search of available hardware?
 *           How practical is this? Certainly, we don't want the programmer
 *           to learn some arbitrary text or XML format for specifying
 *           hardware.(host-names should be dynamic)           
 *            
 *           
 *
 * ARGUMENTS: ("where to look")
 *
 * flags   - EPIPHANY   : in O/S version, this comes from devices driver,
 *                        when runnning ON epiphany, information would need
 *                        to come from somewhere else
 *   
 *           ZYNQ       : O/S provides chip number (needed for bitstream load)
 
 *           GPU        : O/S provided. (magic inside OpenCL?)
 *     
 *           SMP        : O/S provides information about # cores
 *
 *           CLUSTER    : A set of IP addresses (from where????)
 *
 *                          
 * RETURN VALUE:
 *
 * p_dev_t struct pointer
 *
 */

p_dev_t p_init (){

    /*PLACE CODE HERE*/   

    //1.get system information from a kernel driver (or FLAG if simple)
    //2.stuff information into structure p_dev_t
    //3.return pointer to p_dev_t
    //4.exit 
}
