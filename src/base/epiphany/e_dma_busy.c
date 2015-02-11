
#include "e_regs.h"
#include "e_dma.h"

int e_dma_busy(e_dma_id_t chan)
{
	switch (chan)
	{
	case E_DMA_0:
		return e_reg_read(E_REG_DMA0STATUS) & 0xf;
	case E_DMA_1:
		return e_reg_read(E_REG_DMA1STATUS) & 0xf;
	default:
		return -1;
	}
}
