#include <pal.h>

static __inline __attribute((__always_inline__)) float my_hypot( float a, float b );

/**
 * A Sobel 3x3 convolution filter (m) with the Sobel operators defined as:
 *
 *       | -1  0  1 |
 * Gx =  | -2  0  2 | * 1/8
 *       | -1  0  1 |
 *
 *       | -1 -2 -1 |
 * Gy =  |  0  0  0 | * 1/8
 *       |  1  2  1 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 *
 * Gradient Direction (theta) = atan2(Gy,Gx)
 *
 * Notes: cols must be a multiple of 2
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 */

void p_sobel3x3_f32(const float *x, float *r, int rows, int cols)
{
	int i, j;
	float Gx, Gy;
	float s;
	float a00, a01, a02, a03, a04;
	float a10, a11, a12, a13, a14;
	float a20, a21, a22, a23, a24;

	const float* px = x;
	float* pr = r+cols+1;

	for (j = 0; j < (rows - 2); j++) {
		i = 0;
		// Unroll 3x is smaller code than 4x and maybe faster. This loop
		// block may be removed to reduce code size with reduced performance
		for (; i < (cols - 4); i+=3) {
			a00 = px[0];
			a01 = px[1];
			a02 = px[2];
			a03 = px[3];
			a04 = px[4];
			a10 = px[cols];
			a11 = px[cols+1];
			a12 = px[cols+2];
			a13 = px[cols+3];
			a14 = px[cols+4];
			a20 = px[2*cols];
			a21 = px[2*cols+1];
			a22 = px[2*cols+2];
			a23 = px[2*cols+3];
			a24 = px[2*cols+4];
			s = a22 - a00;
			Gx = s + __builtin_fmaf(-2.0f,a10,-a20) + __builtin_fmaf(2.0f,a12,a02);
			Gy = s + __builtin_fmaf(-2.0f,a01,-a02) + __builtin_fmaf(2.0f,a21,a20);
			*(pr++) = my_hypot(Gx, Gy) * M_DIV8;
			s = a23 - a01;
			Gx = s + __builtin_fmaf(-2.0f,a11,-a21) + __builtin_fmaf(2.0f,a13,a03);
			Gy = s + __builtin_fmaf(-2.0f,a02,-a03) + __builtin_fmaf(2.0f,a22,a21);
			*(pr++) = my_hypot(Gx, Gy) * M_DIV8;
			s = a24 - a02;
			Gx = s + __builtin_fmaf(-2.0f,a12,-a22) + __builtin_fmaf(2.0f,a14,a04);
			Gy = s + __builtin_fmaf(-2.0f,a03,-a04) + __builtin_fmaf(2.0f,a23,a22);
			*(pr++) = my_hypot(Gx, Gy) * M_DIV8;
			px += 3;
		}
		// catching remainder (or full loop if loop above is removed)
		for (; i < (cols - 2); i++) {
			a00 = px[0];
			a01 = px[1];
			a02 = px[2];
			a10 = px[cols];
			a11 = px[cols+1];
			a12 = px[cols+2];
			a20 = px[2*cols];
			a21 = px[2*cols+1];
			a22 = px[2*cols+2];
			s = a22 - a00;
			Gx = s + __builtin_fmaf(-2.0f,a10,-a20) + __builtin_fmaf(2.0f,a12,a02);
			Gy = s + __builtin_fmaf(-2.0f,a01,-a02) + __builtin_fmaf(2.0f,a21,a20);
			*(pr++) = my_hypot(Gx, Gy) * M_DIV8;
			px++;
		}
		pr += 2;
		px += 2;
	}
	return;
}

/**
 * Approximates the hypotenuse given two sides of a right triangle using
 * a two Newton iterations for calculating the square root operation. The
 * second iteration can be removed for higher performance and smaller code
 * size at the expense of precision.
 *      /|
 *     / |
 * 's'/  | 'a'
 *   /   |
 *  /____|
 *   'b'
 * s = sqrt (a^2 + b^2)
 *
 * @param a  Length of one side
 *
 * @param b  Length of the second side
 *
 * @return   The length of the hypotenuse
 */

static __inline __attribute((__always_inline__)) float my_hypot( float a, float b )
{
	float s2 = __builtin_fmaf(a,a,b * b);
	float x = s2 * -0.5f;
	long i  = * ( long * ) &s2;
	i  = 0x5f375a86 - ( i >> 1 );
	float y  = * ( float * ) &i;
	y = y * __builtin_fmaf(x, y*y, 1.5f); // 1st Newton iteration
	y = y * __builtin_fmaf(x, y*y, 1.5f); // 2nd iteration, this can be removed
	return s2 * y;
}
