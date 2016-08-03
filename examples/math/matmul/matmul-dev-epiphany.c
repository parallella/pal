/*
  matmul_unit.c

  Copyright (C) 2012 Adapteva, Inc.
  Contributed by Yaniv Sapir <yaniv@adapteva.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program, see the file COPYING.  If not, see
  <http://www.gnu.org/licenses/>.
*/


// This program is the accelerator part of the matmul() example project.
//
// This program runs on the Epiphany system and answers the host with the
// calculation result of the operand matrices.
//
// Aug-2013, YS.

#include <pal.h>

#include "matlib.h"
#include "matmul.h"

#include "static-buffers.h"
#include "common-buffers.h"

void bigmatmul();
void init();
void data_copy(e_dma_desc_t *dma_desc, void *dst, void *src);

p_mutex_t pmutex = P_MUTEX_INITIALIZER;

int main(int argc, char *argv[])
{
	int status;

	status = 0;

	// Initialize data structures - mainly target pointers
	init();

	// Sync with all other cores
	p_barrier(P_TEAM_DEFAULT);

	// Calculate. During this time, the host polls the
	// shared mailbox, waiting for a '3' that
	// indicates the end of the calculation.
	bigmatmul();

	// Sync with all other cores
	p_barrier(P_TEAM_DEFAULT);

	return status;
}


void init()
{
	// Init core enumerations
	me.row     = e_group_config.core_row;
	me.col     = e_group_config.core_col;
	e_neighbor_id(E_PREV_CORE, E_ROW_WRAP,   &me.rowh, &me.colh);
	e_neighbor_id(E_PREV_CORE, E_COL_WRAP,   &me.rowv, &me.colv);
	e_neighbor_id(E_NEXT_CORE, E_GROUP_WRAP, &me.rown, &me.coln);

	// Initialize the mailbox shared buffer pointers
	Mailbox.pBase = (void *) e_emem_config.base;
	Mailbox.pA    = Mailbox.pBase + offsetof(shared_buf_t, A[0]);
	Mailbox.pB    = Mailbox.pBase + offsetof(shared_buf_t, B[0]);
	Mailbox.pC    = Mailbox.pBase + offsetof(shared_buf_t, C[0]);
	Mailbox.pCore = Mailbox.pBase + offsetof(shared_buf_t, core);

	// Initialize per-core parameters - core data structure

	// Initialize pointers to the operand matrices ping-pong arrays
	me.bank_A[_PING] = (void *) &(AA[_PING][0][0]);
	me.bank_A[_PONG] = (void *) &(AA[_PONG][0][0]);
	me.bank_B[_PING] = (void *) &(BB[_PING][0][0]);
	me.bank_B[_PONG] = (void *) &(BB[_PONG][0][0]);
	me.bank_C        = (void *) &(CC       [0][0]);

	// Initialize the pointer addresses of the arrays in the horizontal and vertical target
	// cores, where the submatrices data will be swapped, and the inter-core sync signals.
	me.tgt_A[_PING] = e_get_global_address(me.rowh, me.colh, me.bank_A[_PONG]);
	me.tgt_A[_PONG] = e_get_global_address(me.rowh, me.colh, me.bank_A[_PING]);
	
	me.tgt_B[_PING] = e_get_global_address(me.rowv, me.colv, me.bank_B[_PONG]);
	me.tgt_B[_PONG] = e_get_global_address(me.rowv, me.colv, me.bank_B[_PING]);

	me.pingpong = _PING;


	// Wait for the DMA engine to be idle
	e_dma_wait(E_DMA_0);
	// Tehen, prepare the DMA descriptors
	dma_desc[0].config       = E_DMA_MSGMODE | E_DMA_DWORD | E_DMA_MASTER | E_DMA_ENABLE;
	dma_desc[0].inner_stride = (0x0008 << 16) | 0x0008;
	dma_desc[0].count        = (_Score << 16) | (_Score >> 1);
	dma_desc[0].outer_stride = (0x0008 << 16) | (((_Smtx - _Score) * sizeof(float)) + 0x0008);

	// Duplicate descriptor twice and make necessary corrections for outer strides
	dma_desc[1] = dma_desc[0];
	dma_desc[1].outer_stride = (0x0008 << 16) | 0x0008;
	dma_desc[2] = dma_desc[0];
	dma_desc[2].outer_stride = ((((_Smtx - _Score) * sizeof(float)) + 0x0008) << 16) | 0x0008;

	return;
}


void bigmatmul()
{
	int  im, jm, km; // index of chip array (chip) (0..#arrays-in-matrix)
	int  ic, jc, kc; // index of core in a chip (0..Nside)
	void *src, *dst; // source and destination addresses for DMA transfers

	// Chip loop through operand matrix:
	// Smtx is the size of operand matrices (Smtx x Smtx)
	// Schip is size of a chip matrix (Schip x Schip)
	for (im=0; im<_Smtx; im+=_Schip)
	{
		for (jm=0; jm<_Smtx; jm+=_Schip)
		{
			// First clear the local result submatrix. The product result will be
			// integrated into this submatrix.
			matclr(me.bank_C, _Score);

			for (km=0; km<_Smtx; km+=_Schip)
			{
				// Core loop through chip:
				// for every chip (mesh) iteration on the operand matrix
				// calculate the matmul of the chip-sized submatrices
				// in granularity of cores

				// Wait for the DMA token
				p_mutex_lock(&pmutex);

				// get A block from external DRAM
				ic = me.row * _Score;
				jc = ((me.col + me.row) % _Nside) * _Score;

				src = &(Mailbox.pA[(im+ic)*_Smtx + (km+jc)]);
				dst = me.bank_A[me.pingpong];

				// Read the data
				data_copy(&dma_desc[0], dst, src);

				// get B block from DRAM
				jc = me.col * _Score;
				ic = ((me.row + me.col) % _Nside) * _Score;

				src = &(Mailbox.pB[(km+ic)*_Smtx + (jm+jc)]);
				dst = me.bank_B[me.pingpong];

				// Read the data
				data_copy(&dma_desc[0], dst, src);

				// Pass the DMA token to next core
				p_mutex_unlock(&pmutex);

				// Multiply submatrices (inner product of row x column)
				for (kc=0; kc<_Nside; kc++)
				{
					// Core matmul:
					// for every core calculate the matmul
					// of its sample elements and accumulate with
					// previous partial products
					matmac(me.bank_A[me.pingpong], me.bank_B[me.pingpong], me.bank_C, _Score);

					// After multiplying the submatrices in each core, rotate the rows of A and columns of B
					// If this is the last iteration of the inner product, skip the matrix swap
					//
					// Swap A banks horizontally
					src = me.bank_A[me.pingpong];
					dst = me.tgt_A[me.pingpong];
					if (kc < (_Nside - 1)) 
						data_copy(&dma_desc[1], dst, src);

					// Swap B banks vertically
					src = me.bank_B[me.pingpong];
					dst = me.tgt_B[me.pingpong];
					if (kc < (_Nside - 1))
						data_copy(&dma_desc[1], dst, src);

					me.pingpong = 1 - me.pingpong;

					// Sync with all other cores
					p_barrier(P_TEAM_DEFAULT);
				}
			}

			// Write the core's result to DRAM
			ic = me.row * _Score;
			jc = me.col * _Score;

			src = me.bank_C;
			dst = &(Mailbox.pC[(im+ic)*_Smtx + (jm+jc)]);

			// Wait for the DMA token
			p_mutex_lock(&pmutex);

			// Write data
			data_copy(&dma_desc[2], dst, src);

			// Pass the DMA token to the next core
			p_mutex_unlock(&pmutex);
		}
	}

	return;
}


// Use DMA to copy data blocks from src to dst
void data_copy(e_dma_desc_t *dma_desc, void *dst, void *src)
{
	// Make sure DMA is inactive before modifying the descriptor
	e_dma_wait(E_DMA_0);
	dma_desc->src_addr = src;
	dma_desc->dst_addr = dst;

	e_dma_start(dma_desc, E_DMA_0);

	// All DMA transfers are blocking, so wait for process to finish
	e_dma_wait(E_DMA_0);

	return;
}

