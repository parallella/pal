#include <pal.h>

#include <stdio.h>

#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * A memory fence. Ensures that all read/write operations
 * from the processor exeucting the p_fence call have completed.
 *
 *
 */

void p_fence(void)
{
	__sync_synchronize ();
}
