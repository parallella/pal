
#include "e_lib.h"

void e_irq_global_mask(e_bool_t state)
{
	switch (state)
	{
	case E_FALSE:
		asm("gie");
		break;
	case E_TRUE:
		asm("gid");
		break;
	}

	return;
}
