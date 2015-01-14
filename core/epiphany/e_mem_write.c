
#include <string.h>
#include "e_coreid.h"
#include "e_mem.h"

void *e_write(const void *remote, const void *src, unsigned row, unsigned col, void *dst, size_t n)
{
	void *gdst;

	if (*((e_objtype_t *) remote) == E_EPI_GROUP) {
		gdst = e_get_global_address(row, col, dst);
	} else if (*((e_objtype_t *) remote) == E_SHARED_MEM) {
		// dst is ignored for shared memory writes
		e_memseg_t *pmem = (e_memseg_t *)remote;
		gdst = (void*)pmem->ephy_base;
	} else {
		gdst = (void *) (e_emem_config.base + (unsigned) dst);
	}

	memcpy(gdst, src, n);

	return gdst;
}
