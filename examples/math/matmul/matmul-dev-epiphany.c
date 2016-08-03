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
#include <string.h>

#include "matlib.h"
#include "matmul.h"

#include "static-buffers.h"
#include "common-buffers.h"

void bigmatmul();
void init();
void data_copy_2d(void *dst,
				  const void *src,
				  unsigned inner_count,
				  unsigned outer_count,
				  unsigned inner_stride,
				  unsigned dst_outer_stride,
				  unsigned src_outer_stride);

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
	int32_t eram_base;
	p_coords_t tmp_coords;
	p_dev_t dev;

	// TODO: Shouldn't need this when started as accelerator/device/slave,
	// but we need dev for the p_query call.
	dev = p_init(P_DEV_EPIPHANY, 0);
	eram_base = p_query(dev, P_PROP_MEMBASE);

	me.rank = p_team_rank(P_TEAM_DEFAULT);
	p_rank_to_coords(P_TEAM_DEFAULT, me.rank, &me.coords, 0);

	tmp_coords.row = -1;
	tmp_coords.col = 0;
	me.north_rank = p_rel_coords_to_rank(P_TEAM_DEFAULT, me.rank, &tmp_coords,
										 P_COORDS_WRAP);
	tmp_coords.row = 0;
	tmp_coords.col = -1;
	me.west_rank = p_rel_coords_to_rank(P_TEAM_DEFAULT, me.rank, &tmp_coords,
										P_COORDS_WRAP);

	// Initialize the mailbox shared buffer pointers
	Mailbox.pBase = (void *) eram_base;
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
	// TODO: What's the size here (last arg) ?
	me.tgt_A[_PING] = p_map_member(P_TEAM_DEFAULT, me.west_rank,
								   (unsigned long) me.bank_A[_PONG], 0x4000);
	me.tgt_A[_PONG] = p_map_member(P_TEAM_DEFAULT, me.west_rank,
								   (unsigned long) me.bank_A[_PING], 0x4000);

	me.tgt_B[_PING] = p_map_member(P_TEAM_DEFAULT, me.north_rank,
								   (unsigned long) me.bank_B[_PONG], 0x4000);
	me.tgt_B[_PONG] = p_map_member(P_TEAM_DEFAULT, me.north_rank,
								   (unsigned long) me.bank_B[_PING], 0x4000);

	me.pingpong = _PING;

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
				ic = me.coords.row * _Score;
				jc = ((me.coords.col + me.coords.row) % _Nside) * _Score;

				src = &(Mailbox.pA[(im+ic)*_Smtx + (km+jc)]);
				dst = me.bank_A[me.pingpong];

				// Read the data
				data_copy_2d(dst, src, (_Score >> 1), _Score, 8, 8, ((_Smtx - _Score) * sizeof(float) + 8));

				// get B block from DRAM
				jc = me.coords.col * _Score;
				ic = ((me.coords.row + me.coords.col) % _Nside) * _Score;

				src = &(Mailbox.pB[(km+ic)*_Smtx + (jm+jc)]);
				dst = me.bank_B[me.pingpong];

				// Read the data
				data_copy_2d(dst, src, (_Score >> 1), _Score, 8, 8, ((_Smtx - _Score) * sizeof(float) + 8));

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
						data_copy_2d(dst, src, (_Score >> 1), _Score, 8, 8, 8);

					// Swap B banks vertically
					src = me.bank_B[me.pingpong];
					dst = me.tgt_B[me.pingpong];
					if (kc < (_Nside - 1))
						data_copy_2d(dst, src, (_Score >> 1), _Score, 8, 8, 8);

					me.pingpong = 1 - me.pingpong;

					// Sync with all other cores
					p_barrier(P_TEAM_DEFAULT);
				}
			}

			// Write the core's result to DRAM
			ic = me.coords.row * _Score;
			jc = me.coords.col * _Score;

			src = me.bank_C;
			dst = &(Mailbox.pC[(im+ic)*_Smtx + (jm+jc)]);

			// Wait for the DMA token
			p_mutex_lock(&pmutex);

			// Write data
			data_copy_2d(dst, src, (_Score >> 1), _Score, 8, ((_Smtx - _Score) * sizeof(float) + 8), 8);

			// Pass the DMA token to the next core
			p_mutex_unlock(&pmutex);
		}
	}

	return;
}

void data_copy_2d(void *dst,
				  const void *src,
				  unsigned inner_count,
				  unsigned outer_count,
				  unsigned inner_stride,
				  unsigned dst_outer_stride,
				  unsigned src_outer_stride)
{
	uintptr_t src_addr, dst_addr;

	src_addr    = (uintptr_t) src;
	dst_addr    = (uintptr_t) dst;

	while (outer_count--) {
		memcpy((void *) dst_addr, (void *) src_addr,
			   inner_stride * inner_count);

		dst_addr += inner_stride * (inner_count - 1);
		src_addr += inner_stride * (inner_count - 1);

		dst_addr += dst_outer_stride;
		src_addr += src_outer_stride;
	}

	return;
}
