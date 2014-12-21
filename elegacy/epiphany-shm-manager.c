

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <err.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <linux/epiphany.h>
#include <memman.h>

#include "epiphany-hal.h"
#include "epiphany-hal-api-local.h"
#include "epiphany-shm-manager.h"

static e_shmtable_t    *shm_table        = 0;
static size_t           shm_table_length = 0;
static int              epiphany_devfd   = 0;
static epiphany_alloc_t shm_alloc        = { 0 };

static e_shmseg_pvt_t* shm_lookup_region(const char *name);
static e_shmseg_pvt_t* shm_alloc_region(const char *name, size_t size);
static int shm_table_sanity_check(e_shmtable_t *tbl);

static int shm_lock_file(const int fd, const char* fn);
static int shm_unlock_file(const int fd, const char* fn);

/* Convenience macros */
#define LOCK_SHM_TABLE() shm_lock_file(epiphany_devfd, __func__)
#define UNLOCK_SHM_TABLE() shm_unlock_file(epiphany_devfd, __func__)

extern int	 e_host_verbose;
#define diag(vN) if (e_host_verbose >= vN)


/**
 * Initialize the shared memory manager.
 */
int e_shm_init()
{
	int              devfd       = 0;
	unsigned int     heap        = 0;
	unsigned int     heap_length = 0;

	/* Map the epiphany global shared memory into process address space */
	devfd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
	if ( -1 == devfd ) {
		warnx("e_init(): EPIPHANY_DEV file open failure.");
		return E_ERR;
	}
	epiphany_devfd = devfd;

	memset(&shm_alloc, 0, sizeof(shm_alloc));
	if ( -1 == ioctl(devfd, EPIPHANY_IOC_GETSHM, &shm_alloc) ) {
		warnx("e_shm_init(): Failed to obtain the global "
			  "shared memory. Error is %s", strerror(errno));
		return E_ERR;
	}
	shm_table_length = shm_alloc.size;

	shm_alloc.uvirt_addr = (unsigned long)mmap(0, shm_alloc.size,
		PROT_READ|PROT_WRITE, MAP_SHARED, devfd, (off_t)shm_alloc.mmap_handle);
	if ( MAP_FAILED == (void*)shm_alloc.uvirt_addr ) {
		warnx("e_shm_init(): Failed to map global shared memory. Error is %s",
			  strerror(errno));
		return E_ERR;
	}

	diag(H_D1) { fprintf(stderr, "e_shm_init(): mapped shm: handle 0x%08lx, "
						 "uvirt 0x%08lx, size 0x%08lx\n", shm_alloc.mmap_handle,
						 shm_alloc.uvirt_addr, shm_alloc.size); }

	/** The shm table is initialized by the Epiphany driver. */
	shm_table = (e_shmtable_t*)shm_alloc.uvirt_addr;


	// Enter critical section
	if ( E_OK != LOCK_SHM_TABLE() )
		return E_ERR;

	/* Check whether we have a working SHM table and if not reset it */
	if ( E_OK != shm_table_sanity_check(shm_table) ) {
		if (shm_table->initialized)
			warnx("e_shm_init(): SHM table was corrupted. Will reset it.");

		memset((void *) shm_table, 0, sizeof(*shm_table));
		shm_table->magic      = SHM_MAGIC;
		shm_table->paddr_epi  = shm_alloc.bus_addr;
		shm_table->paddr_cpu  = shm_alloc.phy_addr;

		shm_table->initialized = 1;
		diag(H_D1) { fprintf(stderr, "e_shm_init(): SHM table was reset.\n"); }
	}

	/* Finally, calculate heap base and heap size and initialize memory
	 * manager */
	heap = shm_alloc.uvirt_addr + sizeof(*shm_table);
	heap_length = GLOBAL_SHM_SIZE - (heap - shm_alloc.uvirt_addr);

	diag(H_D1) { fprintf(stderr, "e_shm_init(): initializing memory manager."
						 " Heap addr is 0x%08x, length is 0x%08x\n",
						 heap, heap_length); }

	memman_init((void*)heap, heap_length);


	if ( E_OK != UNLOCK_SHM_TABLE() )
		return E_ERR;

	return E_OK;
}

void e_shm_finalize(void)
{
	munmap((void*)shm_table, shm_table_length);
	diag(H_D2) { fprintf(stderr, "e_shm_finalize(): teardown complete\n"); }
}

int e_shm_alloc(e_mem_t *mbuf, const char *name, size_t size)
{
	e_shmtable_t   *tbl	   = NULL;
	e_shmseg_pvt_t *region = NULL; 
	int				retval = E_ERR;

	if ( !mbuf || !name || !size ) {
		errno = EINVAL;
		goto err2;
	}

	// Enter critical section
	if ( E_OK != LOCK_SHM_TABLE() )
		goto err2;

	tbl = e_shm_get_shmtable();

	if ( E_OK != shm_table_sanity_check(tbl) ) {
		errno = EINVAL;
		goto err1;
	}

	if ( shm_lookup_region(name) ) {
		errno = EEXIST;
		goto err1;
	}

	diag(H_D1) { fprintf(stderr, "e_shm_alloc(): alloc request for 0x%08x "
						 "bytes named %s\n", size, name); }

	region = shm_alloc_region(name, size);
	if ( region ) {
		region->valid = 1;
		region->refcnt = 1;

		mbuf->objtype = E_SHARED_MEM;
		mbuf->memfd = epiphany_devfd;
		mbuf->phy_base = tbl->paddr_cpu;
		mbuf->ephy_base = tbl->paddr_epi;
		mbuf->page_base = 0; // Not used for shared memory regions
		mbuf->page_offset = region->shm_seg.offset;
		mbuf->map_size = region->shm_seg.size;
		mbuf->mapped_base = ((char*)(tbl));
		mbuf->base = mbuf->mapped_base + mbuf->page_offset;
		mbuf->emap_size = region->shm_seg.size;

		retval = E_OK;
	} else {
		diag(H_D1) { fprintf(stderr, "e_shm_alloc(): alloc request for 0x%08x "
							 "bytes named %s failed\n", size, name); }

		errno = ENOMEM;
	}

 err1:
	// Exit critical section
	if ( E_OK != UNLOCK_SHM_TABLE() )
		retval = E_ERR;

 err2:
	return retval;
}

int e_shm_attach(e_mem_t *mbuf, const char *name)
{
	e_shmtable_t   *tbl	   = NULL;
	e_shmseg_pvt_t *region = NULL;
	int				retval = E_ERR;

	if ( !mbuf || !name ) {
		return E_ERR;
	}

	// Enter critical section
	if ( E_OK != LOCK_SHM_TABLE() )
		return E_ERR;

	tbl = e_shm_get_shmtable();

	if ( E_OK != shm_table_sanity_check(tbl) ) {
		retval = E_ERR;
		goto err;
	}

	region = shm_lookup_region(name);
	if ( region ) {
		++region->refcnt;

		mbuf->objtype = E_SHARED_MEM;
		mbuf->memfd = epiphany_devfd;
		mbuf->phy_base = tbl->paddr_cpu + sizeof(*tbl);
		mbuf->ephy_base = tbl->paddr_epi + sizeof(*tbl); 
		mbuf->page_base = 0; // Not used ??
		mbuf->page_offset = region->shm_seg.offset;
		mbuf->map_size = region->shm_seg.size;
		mbuf->mapped_base = ((char*)(tbl));
		mbuf->base = mbuf->mapped_base + mbuf->page_offset;
		mbuf->emap_size = region->shm_seg.size;

		retval = E_OK;
	}

 err:
	// Exit critical section
	if ( E_OK != UNLOCK_SHM_TABLE() )
		return E_ERR;

	return retval;
}

int e_shm_release(const char *name)
{
	e_shmseg_pvt_t   *region = NULL;
	int               retval = E_ERR;

	// Enter critical section
	if ( E_OK != LOCK_SHM_TABLE() )
		return E_ERR;

	region = shm_lookup_region(name);

	if ( region ) {
		if ( 0 == --region->refcnt ) {
			region->valid = 0;
			memman_free(region->shm_seg.addr);
		}
		retval = E_OK;
	}

	// Exit critical section
	if ( E_OK != UNLOCK_SHM_TABLE() )
		return E_ERR;

	return retval;
}

e_shmtable_t* e_shm_get_shmtable(void)
{
	return shm_table;
}


/**
 * Search the shm table for a region named by name.
 *
 * WARNING: The caller should hold the shm table lock when
 * calling this function.
 */
static e_shmseg_pvt_t*
shm_lookup_region(const char *name)
{
	e_shmseg_pvt_t   *retval = NULL;
	e_shmtable_t     *tbl    = NULL;
	int               i      = 0;

	tbl = e_shm_get_shmtable();
	
	for ( i = 0; i < MAX_SHM_REGIONS; ++i ) {
		if ( tbl->regions[i].valid && 
			 !strcmp(name, tbl->regions[i].shm_seg.name) ) {
			retval = &tbl->regions[i];
			break;
		}
	}

	return retval;
}

/**
 * Search the shm table for a region named by name.
 *
 * WARNING: The caller should hold the shm table lock when
 * calling this function.
 */
static e_shmseg_pvt_t*
shm_alloc_region(const char *name, size_t size)
{
	e_shmseg_pvt_t   *region = NULL;
	e_shmtable_t     *tbl    = NULL;
	int               i      = 0;

	tbl = e_shm_get_shmtable();

	for ( i = 0; i < MAX_SHM_REGIONS; ++i ) {
		if ( !tbl->regions[i].valid ) {

			region = &tbl->regions[i];
			strncpy(region->shm_seg.name, name, sizeof(region->shm_seg.name));

			region->shm_seg.addr = memman_alloc(size);
			if ( !region->shm_seg.addr ) {
				/* Allocation failed */
				diag(H_D1) { fprintf(stderr, "shm_alloc_region(): alloc request for 0x%08x "
									 "bytes named %s failed\n", size, name); }

				region = NULL;
				break;
			}

			/*
			 * Note: the shm heap follows the shm table in memory.
			 */
			region->shm_seg.offset = ((char*)region->shm_seg.addr) -
				((char*)tbl);

			region->shm_seg.paddr = ((char*)tbl->paddr_epi) + 
				region->shm_seg.offset;
			
			region->shm_seg.size = size;

			tbl->regions[i].valid = 1;

			diag(H_D1) {
				fprintf(stderr, "e_hal::shm_alloc_region(): allocated shm "
						"region: name %s, addr 0x%08lx, paddr 0x%08lx, "
						"offset 0x%08x, size 0x%08x\n", region->shm_seg.name,
						(unsigned long)region->shm_seg.addr,
						(unsigned long)region->shm_seg.paddr,
						(unsigned)region->shm_seg.offset,
						region->shm_seg.size);
			}
			
			break;
		}
	}

	return region;
}

/**
 * Sanity-check the shm table
 *
 * WARNING: The caller should hold the shm table lock when
 * calling this function.
 */
static int shm_table_sanity_check(e_shmtable_t *tbl)
{
	if ( !tbl ) {
		assert(!"Global shm table is NULL, did you forget to call"
			   " e_shm_init()?");
		return E_ERR;
	}

	if ( !tbl->initialized ) {
		warnx("shm_table_sanity_check(): shm table is not initialized.");
		return E_ERR;
	}

	if ( tbl->magic != SHM_MAGIC ) {
		warnx("shm_table_sanity_check(): Bad shm magic. "
			  "Expected 0x%08x found 0x%08x",
			  SHM_MAGIC, tbl->magic);
		return E_ERR;
	}

	return E_OK;
}



/**
 * The belows two functions provide mutual exclusion to the
 * SHM table.
 *
 * lockf() is good for simple things like these, but if we ever need more
 * advanced locking features we might have to resort to fcntl().
 */

static int shm_lock_file(const int fd, const char* fn)
{
	diag(H_D3) { fprintf(stderr, "shm_lock_file(): Taking lock...\n"); }
	if ( lockf(fd, F_LOCK, 0) ) {
		warnx("%s(): Failed to lock shared memory. Error is %s",
				fn, strerror(errno));
		return E_ERR;
	}
	diag(H_D3) { fprintf(stderr, "shm_lock_file(): Lock acquired.\n"); }
	return E_OK;
}

static int shm_unlock_file(const int fd, const char* fn)
{
	if ( lockf(fd, F_ULOCK, 0) ) {
		warnx("%s(): Failed to unlock shared memory. Error is %s",
				fn, strerror(errno));
		return E_ERR;
	}
	return E_OK;
}


