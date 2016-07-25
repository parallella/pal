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

#include "static-buffers.h"

// Here's an example of explicit placement of static objects in memory. The three matrices
// are placed in the respective SRAM banks. However, if necessary, the linker may relocate
// the objects wherever within the bank. The core structure "me" is specifically located
// at an explicit address - 0x7000. To do that, a custom linker file (LDF) was defined,
// based on a standard LDF, in which a special data section was added with the required
// address to assign to the "me" object.
volatile float  AA[2][_Score][_Score] SECTION(".data_bank1");  // local A submatrix
volatile float  BB[2][_Score][_Score] SECTION(".data_bank2");  // local B submatrix
volatile float  CC   [_Score][_Score] SECTION(".data_bank3");  // local C submatrix

volatile e_mutex_t    mutex             SECTION("section_core"); // groupe lock mutex
volatile e_barrier_t  barriers[_Ncores] SECTION("section_core"); // barriers array
         e_barrier_t *tgt_bars[_Ncores] SECTION("section_core"); // barriers array
         e_dma_desc_t dma_desc[3]       SECTION("section_core") ALIGN(8); // TCB structure for DMA
         core_t me                      SECTION("section_core"); // core data structure
volatile shared_buf_ptr_t Mailbox       SECTION("section_core"); // Mailbox pointers;
