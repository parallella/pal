

#include <string.h>
#include "e_coreid.h"
#include "e_mem.h"

void *e_read(const void *remote, void *dst, unsigned row, unsigned col, const void *src, size_t n)
{
	void *gsrc;

	if (*((e_objtype_t *) remote) == E_EPI_GROUP) {
		gsrc = e_get_global_address(row, col, src);
	} else if (*((e_objtype_t *) remote) == E_SHARED_MEM) {
		// src is ignored for shared memory reads
		e_memseg_t *pmem = (e_memseg_t *)remote;
		gsrc = (void*)pmem->ephy_base;
	} else {
		gsrc = (void *) (e_emem_config.base + (unsigned) src);
	}

	memcpy(dst, gsrc, n);

	return gsrc;
}
