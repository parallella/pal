
#include "e_regs.h"
#include "e_dma.h"

void e_dma_wait(e_dma_id_t chan)
{
	unsigned dma_busy;

	do {
			if (chan == E_DMA_0)
				dma_busy = e_reg_read(E_REG_DMA0STATUS) & 0xf;
			else
				dma_busy = e_reg_read(E_REG_DMA1STATUS) & 0xf;
	} while (dma_busy);

	return;
}
