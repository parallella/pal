
#ifndef __E_HAL_API_H__
#define __E_HAL_API_H__

#include <sys/types.h>
#include "epiphany-shm-manager.h"

#ifdef __cplusplus
extern "C"
{
#endif

/////////////////////////////////
// Device communication functions
//
// Platform configuration
int		e_init(char *hdf);
int		e_get_platform_info(e_platform_t *platform);
int		e_finalize(void);
// Epiphany access
int		e_open(e_epiphany_t *dev, unsigned row, unsigned col, unsigned rows, unsigned cols);
int		e_close(e_epiphany_t *dev);
// External memory access
int		e_alloc(e_mem_t *mbuf, off_t base, size_t size);
int		e_free(e_mem_t *mbuf);

//
// Data transfer
ssize_t e_read(void *dev, unsigned row, unsigned col, off_t from_addr, void *buf, size_t size);
ssize_t e_write(void *dev, unsigned row, unsigned col, off_t to_addr, const void *buf, size_t size);


///////////////////////////
// System control functions
#define e_reset e_reset_system
int		e_reset_system(void);
int		e_reset_chip(void);
int		e_reset_group(e_epiphany_t *dev);
int		e_start(e_epiphany_t *dev, unsigned row, unsigned col);
int		e_start_group(e_epiphany_t *dev);
int		e_signal(e_epiphany_t *dev, unsigned row, unsigned col);
int		e_halt(e_epiphany_t *dev, unsigned row, unsigned col);
int		e_resume(e_epiphany_t *dev, unsigned row, unsigned col);

////////////////////////////////////////////
// Shared Memory Manager function prototypes

/**
 * Allocate a shared region identifiable by name.
 *
 * @param mbuf - a pointer to an instance of e_mem_t that, upon
 * sucessful return will describe the allocated shared memory region.
 * @param name - the region name. The maximum name length
 * is 256 characters.
 * @param size - the length of the shared region
 *
 * @return E_OK on success, E_ERR on failure. On failure errno will be 
 * set to one of the following:
 *
 * ENOMEM - no free regions are available to satisfy the
 *	   request.
 * EEXIST - a shared region with name already exists
 */
int e_shm_alloc(e_mem_t *mbuf, const char *name, size_t size);

/**
 * Attach to a shared region identifiable by name
 *
 * @param mbuf - a pointer to an instance of e_mem_t that, upon
 * sucessful return will describe the attached shared memory region.
 * @param name - the name of the shared region to attach to.
 * the region must have been allocated with e_shm_alloc
 *
 * @return E_OK on success, E_ERR on failure
 * (a region with name does nt exist).
 */
int e_shm_attach(e_mem_t *mbuf, const char *name);

/**
 * Free a shared memory region, given by name, that was allocated
 * via e_shm_alloc
 *
 * @param name The name of the region to release
 *
 * @return E_OK on success, E_ERR if a region with name
 * does not exist. 
 */
int e_shm_release(const char *name);

/**
 * Returns a pointer to the global shared memory table
 */
e_shmtable_t* e_shm_get_shmtable(void);

////////////////////
// Utility functions
unsigned e_get_num_from_coords(e_epiphany_t *dev, unsigned row, unsigned col);
void	 e_get_coords_from_num(e_epiphany_t *dev, unsigned corenum, unsigned *row, unsigned *col);
//
e_bool_t e_is_addr_on_chip(void *addr);
e_bool_t e_is_addr_on_group(e_epiphany_t *dev, void *addr);
//
e_hal_diag_t e_set_host_verbosity(e_hal_diag_t verbose);

#ifdef __cplusplus
}
#endif

#endif // __E_HAL_API_H__

