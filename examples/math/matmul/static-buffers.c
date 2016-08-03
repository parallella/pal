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
volatile float  AA[2][_Score][_Score] __attribute__((section(".data_bank1")));  // local A submatrix
volatile float  BB[2][_Score][_Score] __attribute__((section(".data_bank2")));  // local B submatrix
volatile float  CC   [_Score][_Score] __attribute__((section(".data_bank3")));  // local C submatrix

core_t me                      __attribute__((section("section_core"))); // core data structure
volatile shared_buf_ptr_t Mailbox       __attribute__((section("section_core"))); // Mailbox pointers;
