
#include "e_regs.h"
#include "e_coreid.h"

void e_reg_write(e_core_reg_id_t reg_id, unsigned val)
{
	register volatile unsigned reg_val = val;
	unsigned *addr;

	// TODO: function affects integer flags. Add special API for STATUS
	switch (reg_id)
	{
	case E_REG_CONFIG:
		__asm__ __volatile__ ("MOVTS CONFIG, %0" : : "r" (reg_val));
		break;
	case E_REG_STATUS:
		__asm__ __volatile__ ("MOVTS STATUS, %0" : : "r" (reg_val));
		break;
	default:
		addr = (unsigned *) e_get_global_address(e_group_config.core_row, e_group_config.core_col, (void *) reg_id);
		*addr = val;
		break;
	}

	return;
}

