/*
  matlib.h

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


#ifndef __MATLIB_H__
#define __MATLIB_H__
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPS (0.00001 * 4)

#define __PROVIDE_NAIVE_MATH__x

void matclr(volatile float * restrict a, int NN);
void matmac(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN);
void matmac_naive(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN);
void subcpy(volatile float * restrict a, int Na, int nai, int naj, volatile float * restrict b, int Nb, int nbi, int nbj, int nn);
void subcpy64(volatile float * restrict a, int Na, int nai, int naj, volatile float * restrict b, int Nb, int nbi, int nbj, int nn);

#ifdef __HOST__
void matadd(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN);
void matmul(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN);
void matsub(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN);
int  iszero(volatile float * restrict a, int NN);
#endif // __HOST__

#endif // __MATLIB_H__
