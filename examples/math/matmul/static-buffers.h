/*
  static_buffers.c

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

#ifndef __STATIC_BUFFERS_H__
#define __STATIC_BUFFERS_H__

#include <e-lib.h>
#include "matmul.h"

extern volatile float AA[2][_Score][_Score];    // local A submatrix
extern volatile float BB[2][_Score][_Score];    // local B submatrix
extern volatile float CC   [_Score][_Score];    // local C submatrix

extern volatile e_mutex_t    mutex;             // groupe lock mutex
extern volatile e_barrier_t  barriers[_Ncores]; // barriers array
extern          e_barrier_t *tgt_bars[_Ncores]; // barriers array
extern          e_dma_desc_t dma_desc[3];       // descriptor structure for DMA
extern          core_t me;                      // core data structure
extern volatile shared_buf_ptr_t Mailbox;       // Mailbox pointers;

#endif // __STATIC_BUFFERS_H__

