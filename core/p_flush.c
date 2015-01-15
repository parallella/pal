/*
 *
 * FUNCTION:
 * p_flush(): broadcasts an array of bytes from source to a list of 
 *            destination addresses
 *
 * ARGUMENTS:
 * dst      - remote destination pointer
 */
#include "pal_core.h"
#include "pal_private.h"

void p_flush (p_mem_t mem){

    unsigned int tmpvar;
    //1.Read data from remote location to local temp variable (READ)
    
    //2.XOR the temporary variable
    tmpvar = tmpvar ^ 0xFFFFFFFF;

    //3.Write inverted data back to remote address (WRITE)
    
    //4.Read remote data until (READ)
    while(1){
	//read..
    }   

    return(0);
}
