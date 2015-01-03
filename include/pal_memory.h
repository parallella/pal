/*
 ***********************************************************************
 * LOW LEVEL MULTICORE MEMORY MANAGEMENT (USER SPACE)
 *
 ***********************************************************************
 */

/*Do we need the dev_t and mem_t at this level???*/
/*physical vs virtual*/
/*should work on epiphany and arm*/
/*adding one level of redirection should resolve this*/
/*make the global address a different type?*/

/*Copies from one global source addr to another  global destination addr*/
void *p_copy(void *dst, const void *src, size_t nb, int flags);

/*Writes to a global memory address from a local address*/
void *p_write(void *dst, const void *loc_src, size_t nb, int flags);

/*Reads from a global memory address */
void *p_read(void *loc_dst, const void *src, size_t nb, int flags);    

/*Scatters an array based on a list of destination pointers*/
void *p_scatter(void *dstlist, const void *loc_src, size_t nb, int nd, int flags);

/*Gathers an array based on a list of source pointers*/
void *p_gather(void *loc_dst, const void *srclist, size_t nb, int ns, int flags);

/*Broadcasts an array based to a list of destination pointers*/
void *p_bcast(void *dstlist, const void *loc_src, size_t nb, int nd, int flags););

/*Allocates dynamic memory*/
void *p_malloc(size_t size););

/*Frees dynamic memory*/
void p_free(void *ptr);

/*Flushes the read/write path to a specific memory location*/
void p_flush(void *ptr);

