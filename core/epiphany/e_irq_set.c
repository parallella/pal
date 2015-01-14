

#include "e_regs.h"
#include "e_coreid.h"
#include "e_ic.h"

void e_irq_set(unsigned row, unsigned col, e_irq_type_t irq)
{
	unsigned *ilatst;

//	if ((row == E_SELF) || (col == E_SELF))
//		ilatst = (unsigned *) E_ILATST;
//	else
	ilatst = (unsigned *) e_get_global_address(row, col, (void *) E_REG_ILATST);

	*ilatst = 1 << (irq - E_SYNC);

	return;
}
