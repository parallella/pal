/*
  matlib.c

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


#include <stdint.h>
#include "matlib.h"

// Clear a NNxNN matrix
void matclr(volatile float * restrict a, int NN)
{
	int i;

	for (i=0; i<NN*NN; i++)
		a[i] = 0;

	return;
}


#ifdef __PROVIDE_NAIVE_MATH__
// Multiply and accumulate two NNxNN matrices c = c + a x b
//void matmac_naive(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN)
void matmac(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN)
{
	int i,  j,  k;

	for (i=0; i<NN; i++)
		for (j=0; j<NN; j++)
			for (k=0; k<NN; k++)
				c[i*NN+j] += a[i*NN+k] * b[k*NN+j];
	
	return;
}

#else // __PROVIDE_NAIVE_MATH__


#define K NN
#define N NN

void matmac(volatile float * restrict aa, volatile float * restrict bb, volatile float * restrict cc, int NN)
{
	int i = 0;

	for (i=0; i<NN; i++)
	{
		int j0, j1;

		for (j0=0; j0<NN; j0+=8)
		{
			int x;
			float tot[8];

			for (j1=0; j1<8; j1++)
			{
				tot[j1] = 0.0;
			}

			for (x=0; x<K; x++)
			{
				float tmp = *(aa + (i * K) + x);

				for (j1=0; j1<8; j1+=4)
				{
					float tmp2[4];

					tmp2[0] = *(bb + (x * N) + j0 + j1 + 0);
					tmp2[1] = *(bb + (x * N) + j0 + j1 + 1);
					tmp2[2] = *(bb + (x * N) + j0 + j1 + 2);
					tmp2[3] = *(bb + (x * N) + j0 + j1 + 3);
					tot[j1 + 0] += tmp * tmp2[0];
					tot[j1 + 1] += tmp * tmp2[1];
					tot[j1 + 2] += tmp * tmp2[2];
					tot[j1 + 3] += tmp * tmp2[3];
				}
			}

			for (j1=0; j1<8; j1+=1)
			{
				*(cc + (i * N) + j0+j1) += tot[j1];
			}
		}
	}

	return;
}
#endif // __PROVIDE_NAIVE_MATH__

#ifdef __PROVIDE_SUB_COPY__
// Copy a submatrix NNxNN from NaxNa to NbxNb, sub_b[nbi,nbj] = sub_A[nai,naj]
void subcpy(volatile float * restrict a, int Na, int nai, int naj, volatile float * restrict b, int Nb, int nbi, int nbj, int NN)
{
	int i, j;

	for (i=0; i<NN; i++)
		for (j=0; j<NN; j+=1)
			b[((nbi+i)*Nb)+(nbj+j)] = a[((nai+i)*Na)+(naj+j)];

	return;
}
#endif // __PROVIDE_SUB_COPY__

#ifdef __HOST__
// Add two NNxNN matrices c = a + b
void matadd(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN)
{
	int i, j;

	for (i=0; i<NN; i++)
		for (j=0; j<NN; j++)
			c[i*NN+j] = a[i*NN+j] + b[i*NN+j];
	
	return;
}


// Multiply two NNxNN matrices c = a x b
void matmul(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN)
{
	int i, j, k;

	for (i=0; i<NN; i++)
		for (j=0; j<NN; j++)
		{
			c[i*NN+j] = 0;
			for (k=0; k<NN; k++)
				c[i*NN+j] += a[i*NN+k] * b[k*NN+j];
		}
	
	return;
}

// Subtract two NNxNN matrices c = a - b
void matsub(volatile float * restrict a, volatile float * restrict b, volatile float * restrict c, int NN)
{
	int i, j;

	for (i=0; i<NN; i++)
		for (j=0; j<NN; j++)
			c[i*NN+j] = a[i*NN+j] - b[i*NN+j];
	
	return;
}


// Check if a NNxNN matrix is zero
int iszero(volatile float * restrict a, int NN)
{
	int i, j, z;

	z = 0;
	for (i=0; i<NN; i++)
		for (j=0; j<NN; j++)
			if (fabs(a[i*NN+j]) > EPS)
				z = z | 1;

	return (!z);
}
#else
#endif // __HOST__
