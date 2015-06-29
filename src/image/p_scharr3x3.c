#include <pal.h>

static __inline __attribute((__always_inline__)) float my_hypot( float a, float b );

/*
 * A Scharr 3x3 convolution filter with the following convolution matrix:
 *
 *       |  3  0  -3 |
 * Gx =  | 10  0 -10 | * 1/36
 *       |  3  0  -3 |
 *
 *       |  3  10  3 |
 * Gy =  |  0   0  0 | * 1/36
 *       | -3 -10 -3 |
 *
 * G = sqrt (Gx^2 + Gy^2)
 *
 * Gradient Direction (theta) = atan2(Gy,Gx)
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

void p_scharr3x3_f32(const float *x, float *r, int rows, int cols)
{
	int i, j;
	float Gx, Gy;
	float s;
	float a00, a01, a02;
	float a10, a11, a12;
	float a20, a21, a22;

	const float* px = x;
	float* pr = r+cols+1;
	
	for (j = 0; j < (rows - 2); ++j) {
		for(i = 0; i < (cols - 2); ++i) {
			a00 = px[0];
			a01 = px[1];
			a02 = px[2];
			a10 = px[cols];
			a11 = px[cols+1];
			a12 = px[cols+2];
			a20 = px[2*cols];
			a21 = px[2*cols+1];
			a22 = px[2*cols+2];
			
			Gx = 10.0f*(a10-a12) + 3.0f*(a00+a20-a02-a22);
			Gy = 10.0f*(a01-a21) + 3.0f*(a00+a02-a20-a22);
			*(pr++) = my_hypot(Gx, Gy) * M_DIV6 * M_DIV6;
			++px;
		}
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

static __inline __attribute((__always_inline__)) float my_hypot(float a, float b)
{
    union fl {
        float f;
        long l;
    };
    union fl s2 = { .l = 0 };
    s2.f = __builtin_fmaf(a,a,b * b);
    float x = s2.f * -0.5f;
    union fl i = { .l = 0x5f375a86 - ( s2.l >> 1 ) } ;
    float y  = i.f;
    y = y * __builtin_fmaf(x, y*y, 1.5f); // 1st Newton iteration
    y = y * __builtin_fmaf(x, y*y, 1.5f); // 2nd iteration, this can be removed
    return s2.f * y;
}
