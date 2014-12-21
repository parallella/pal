
#include "e_dma.h"


void e_dma_set_desc(
		e_dma_id_t chan,
		unsigned config,     e_dma_desc_t *next_desc,
		unsigned strd_i_src, unsigned strd_i_dst,
		unsigned count_i,    unsigned count_o,
		unsigned strd_o_src, unsigned strd_o_dst,
		void     *addr_src,  void *addr_dst,
		e_dma_desc_t *desc)
{
	e_dma_wait(chan);
	desc->config       = (((unsigned) next_desc) << 16) | config;
	desc->inner_stride = (strd_i_dst             << 16) | strd_i_src;
	desc->count        = (count_o                << 16) | count_i;
	desc->outer_stride = (strd_o_dst             << 16) | strd_o_src;
	desc->src_addr     = addr_src;
	desc->dst_addr     = addr_dst;

	return;
}

