
/*atomic compare and exchange.This compares the contents of *atom with the contents of *expected and if equal, writes 'desired' into *atom. If they are not equal, the current contents of *atom is written into *expected. 
*/

#include "pal_core.h"
#include "pal_private.h"
int p_atomic_compswap_u32(p_atom_t atom, uint32_t *input, uint32_t desired){

    /*PLACE CODE HERE*/
    return(0);

}
