
#ifndef __EPIPHANY_SHM_MANAGER_H__
#define __EPIPHANY_SHM_MANAGER_H__

/*
** Function prototypes.
*/

/**
 * Initialize the shared memory manager
 * FIXME: this is an internal function - hide it!
 */
int e_shm_init();

/**
 * Teardown the shared memory manager
 * FIXME: this is an internal function - hide it!
 */
void e_shm_finalize(void);

#endif	  /*  __EPIPHANY_SHM_MANAGER_H__ */
