

#include "e_coreid.h"
#include "e_mutex.h"


void e_mutex_lock(unsigned row, unsigned col, e_mutex_t *mutex)
{
	e_mutex_t *gmutex;
	register unsigned coreid, offset;

	coreid = e_get_coreid();
	gmutex = (e_mutex_t *) e_get_global_address(row, col, mutex);
	offset = 0x0;

	do {
		__asm__ __volatile__("testset  %[r0], [%[r1], %[r2]]" : [r0] "+r" (coreid) : [r1] "r" (gmutex), [r2] "r" (offset));
	} while (coreid != 0);

	return;
}
