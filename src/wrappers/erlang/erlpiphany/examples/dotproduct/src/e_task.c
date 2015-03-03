/*
Copyright (C) 2013 Adapteva, Inc.
Contributed by Andreas Olofsson <support@adapteva.com>
               
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program, see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/


/*
This function calculates the sum of product of two vectors located in the Epiphany
local memory.

for (i=0; i<N; i++) {
	 c += a[i] * b[i];
    }
  
*/
#include <stdio.h>
#include <stdlib.h>
#include "e-lib.h"
#include "common.h"

int main(void)
{
  unsigned *a, *b, *c, *d;
  int i;
  
  a    = (unsigned *) 0x2000;//Address of a matrix (transfered here by host)
  b    = (unsigned *) 0x4000;//Address of b matrix (transfered here by host)
  c    = (unsigned *) 0x6000;//Result
  d    = (unsigned *) 0x7000;//Done
  
  //Clear Sum
  (*(c))=0x0;

  //Sum of product calculation
  for (i=0; i<N/CORES; i++){
    (*(c)) += a[i] * b[i];
  }

  //Raising "done" flag
  (*(d)) = 0x00000001;

  //Put core in idle state
  __asm__ __volatile__("idle");
}

