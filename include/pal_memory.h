/*
 ***********************************************************************
 * MEMORY MANAGEMENT
 *
 ***********************************************************************
 */

/*Copies a array from a source address to a destination address*/
void p_copy();

/*Writes to a global memory address */
void p_write();

/*Reads from a global memory address */
void p_read();    

/*Scatters an array based on a list of destination pointers*/
void p_scatter();

/*Gathers an array based on a list of source pointers*/
void p_gather();

/*Broadcasts an array based to a list of destination pointers*/
void p_bcast();

/*Allocates dynamic memory*/
void p_malloc();

/*Frees dynamic memory*/
void p_free();

/*Flushes the read/write path to a specific memory location*/
void p_flush();
