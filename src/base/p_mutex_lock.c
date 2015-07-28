#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_lock(p_mutex_t *m)
{
	while (!__sync_bool_compare_and_swap(m, 0, 1))
		p_cpu_relax();

	return 0;
}
