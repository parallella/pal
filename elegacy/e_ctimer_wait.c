
#include <e_ctimers.h>

void e_wait(e_ctimer_id_t timer, unsigned int clicks)
{
	// Program ctimer and start counting
	e_ctimer_stop(timer);
	e_ctimer_set(timer, clicks);
	e_ctimer_start(timer, E_CTIMER_CLK);

	// Wait until ctimer is idle
	while (e_ctimer_get(timer)) { };

	return;
}
