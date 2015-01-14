

#include "e_regs.h"
#include "e_ic.h"
#include "e_types.h"

void e_irq_mask(e_irq_type_t irq, e_bool_t state)
{
	unsigned previous;

	previous = e_reg_read(E_REG_IMASK);

	if (state)
		e_reg_write(E_REG_IMASK, previous | (  1<<(irq - E_SYNC)));
	else
		e_reg_write(E_REG_IMASK, previous & (~(1<<(irq - E_SYNC))));

	return;
}
