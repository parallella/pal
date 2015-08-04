#include <pal.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_mutex_trylock(p_mutex_t * m)
{
	return !__sync_bool_compare_and_swap(m, 0, 1);
}
