

#include "e_regs.h"
#include "e_coreid.h"

unsigned e_reg_read(e_core_reg_id_t reg_id)
{
	register volatile unsigned reg_val;
	unsigned *addr;

	// TODO: function affects integer flags. Add special API for STATUS
	switch (reg_id)
	{
	case E_REG_CONFIG:
		__asm__ __volatile__ ("MOVFS %0, CONFIG" : "=r" (reg_val) : );
		return reg_val;
	case E_REG_STATUS:
		__asm__ __volatile__ ("MOVFS %0, STATUS" : "=r" (reg_val) : );
		return reg_val;
	default:
		addr = (unsigned *) e_get_global_address(e_group_config.core_row, e_group_config.core_col, (void *) reg_id);
		return *addr;
	}
}
