
#include "e_regs.h"
#include "e_types.h"
#include "e_dma.h"

int e_dma_start(e_dma_desc_t *descriptor, e_dma_id_t chan)
{
	unsigned        start;
	e_return_stat_t ret_val;

	ret_val = E_ERR;

	if ((chan | 1) != 1)
	{
		return E_ERR;
	}

	/* wait for the DMA engine to be idle */
	while (e_dma_busy(chan));

	start = ((int)(descriptor) << 16) | E_DMA_STARTUP;

	switch (chan)
	{
	case E_DMA_0:
		e_reg_write(E_REG_DMA0CONFIG, start);
		ret_val = E_OK;
		break;
	case E_DMA_1:
		e_reg_write(E_REG_DMA1CONFIG, start);
		ret_val = E_OK;
		break;
	}

	return ret_val;
}
