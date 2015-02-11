

#include "e_regs.h"
#include "e_coreid.h"
#include "e_ic.h"

void e_irq_clear(unsigned row, unsigned col, e_irq_type_t irq)
{
	unsigned *ilatcl;

//	if ((row == E_SELF) || (col == E_SELF))
//		ilatcl = (unsigned *) E_ILATCL;
//	else
	ilatcl = (unsigned *) e_get_global_address(row, col, (void *) E_REG_ILATCL);

	*ilatcl = 1 << (irq - E_SYNC);

	return;
}
