
#include <string.h>
#include <stdio.h>
#include "e_types.h"
#include "e_coreid.h"
#include "e_regs.h"
#include "e_shm.h"

#define HOST_RESERVED_MEM_START	 0x8f000000	 /* fast.ldf - shared_dram */
#define SHM_MAGIC				 0xabcdef00

/* 
 * FIXME: The address of the shm_table is hardcoded to the start address of
 * the "shared_dram". This will change once we have a mechanism to tell
 * the Epiphany cores where, in memory, the shm_table resides.
 *
 * The location of External DRAM is normally read from the Hardware Definition
 * file. But here on the Epiphany-side we have no access to the host filesystem.
 * So for now the HOST_RESERVED_MEM_START is set to the address of the shared_dram.
 */
static const e_shmtable_t *const shm_table = (const e_shmtable_t* const)HOST_RESERVED_MEM_START;


/**
 * e_strcmp is implemented here since I don't know if the strcmp
 * in the Epiphany stdlib is working correctly and is "safe" (i.e.,
 * doesn't clobber memory in the shared_dram area.)
 */
static int e_strcmp(const char *str1, const char *str2)
{
	while ( (*str1 != '\0') || (*str2 != '\0') ) {
		if ( *str1 > *str2 ) {
			return 1;
		}

		if ( *str1 < *str2 ) {
			return -1;
		}

		str1++;
		str2++;
	}

	return 0;
}

static int check_shmtable()
{
	int retval = E_OK;

	if ( SHM_MAGIC != shm_table->magic ) {
		retval = E_ERR;
	}

	return retval;
}

static const e_shmseg_pvt_t*
shm_lookup_region(const char *name)
{
	const e_shmseg_pvt_t	 *retval = NULL;
	int						  i		 = 0;

	for ( i = 0; i < MAX_SHM_REGIONS; ++i ) {
		if ( 1 == shm_table->regions[i].valid ) {
			if ( 0 == e_strcmp(name, shm_table->regions[i].shm_seg.name) ) {
				retval = &shm_table->regions[i];
				break;
			}
		}
	}

	return retval;
}


/**
 * WARNING: we cannot serialize access to the shm table from the Epiphany
 * cores so treat the SHM Table as read-only!! 
 */
int e_shm_attach(e_memseg_t *mem, const char* name)
{
	const e_shmseg_pvt_t  *region = NULL;
	int					   retval = E_ERR;

	if ( !mem || !name ) {
		return retval;
	}

	if ( E_OK == check_shmtable() ) {
		region = shm_lookup_region(name);
		if ( region ) {
			mem->objtype	 = E_SHARED_MEM;
			mem->phy_base	 = shm_table->paddr_cpu + region->shm_seg.offset;
			mem->ephy_base	 = (unsigned)(region->shm_seg.paddr);
			mem->size		 = region->shm_seg.size;
			mem->type		 = E_RDWR;

			retval			 = E_OK;
		}
	}

	return retval;
}

int e_shm_release(const char* name)
{
	int retval = E_ERR;
	(void)name;

	/** Nothing to cleanup here at the moment */
	if ( check_shmtable() ) {
		retval = E_OK;
	}

	return retval;
}
