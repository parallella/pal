

#include "e_coreid.h"
#include "e_mutex.h"


void e_mutex_unlock(unsigned row, unsigned col, e_mutex_t *mutex)
{
	e_mutex_t *gmutex;

	gmutex = (e_mutex_t *) e_get_global_address(row, col, mutex);

	*gmutex = 0x0;

	return;
}
