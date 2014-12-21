
#include "e_ctimers.h"
#include "e_regs.h"

unsigned e_ctimer_stop(e_ctimer_id_t timer)
{
// TODO convert to assembly to eliminate 2 function calls.
	unsigned shift;
	unsigned mask;
	unsigned config;
	unsigned count;

	shift = (timer) ? 8:4;
	mask = 0xf << shift;
	config = e_reg_read(E_REG_CONFIG);
	// stop the timer
	e_reg_write(E_REG_CONFIG, config & (~mask));

	count = e_reg_read(timer ? E_REG_CTIMER1 : E_REG_CTIMER0);

	return count;
}

