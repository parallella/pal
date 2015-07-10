#include <pal.h>

#define C0 (3.3333333f) // 10/3
#define C1 (0.09375f)   // 3/32
#define FMA(a,b,c) __builtin_fmaf(a,b,c)

static __inline __attribute((__always_inline__)) float my_hypot( float a, float b );

/*
 * A Scharr 3x3 convolution filter with the following convolution matrix:
 *
 *       |  3  0  -3 |
 * Gx =  | 10  0 -10 | * 1/32
 *       |  3  0  -3 |
 *
 *       |  3  10  3 |
 * Gy =  |  0   0  0 | * 1/32
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
	float a02, a03, a04, a05;
	float a12, a13, a14, a15;
	float a22, a23, a24, a25;
	float cx2, cx3, cx4, cx5;
	float cy2, cy3, cy4, cy5;
	float Gx2, Gx3, Gx4, Gx5;
	float Gy2, Gy3, Gy4, Gy5;

	const float* px = x;
	float* pr = r+cols+1;

	for (j = 0; j < (rows - 2); j++) {
		a04 = px[0];
		a05 = px[1];
		a14 = px[cols+0];
		a15 = px[cols+1];
		a24 = px[2*cols+0];
		a25 = px[2*cols+1];
		cx4 = FMA(C0,a14,a04+a24);
		cx5 = FMA(C0,a15,a05+a25);
		cy4 = a24 - a04;
		cy5 = a25 - a05;
		for (i = 0; i < (cols - 5); i+=4) { // unroll 4x
			a02 = px[2];
			a12 = px[cols+2];
			a22 = px[2*cols+2];
			a03 = px[3];
			a13 = px[cols+3];
			a23 = px[2*cols+3];
			a04 = px[4];
			a14 = px[cols+4];
			a24 = px[2*cols+4];
			a05 = px[5];
			a15 = px[cols+5];
			a25 = px[2*cols+5];
			cx2 = FMA(C0,a12,a02+a22);
			cy2 = a22 - a02;
			Gx2 = cx2 - cx4;
			Gy2 = FMA(C0,cy5,cy4+cy2);
			cx3 = FMA(C0,a13,a03+a23);
			cy3 = a23 - a03;
			Gx3 = cx3 - cx5;
			Gy3 = FMA(C0,cy2,cy5+cy3);
			cx4 = FMA(C0,a14,a04+a24);
			cy4 = a24 - a04;
			Gx4 = cx4 - cx2;
			Gy4 = FMA(C0,cy3,cy2+cy4);
			cx5 = FMA(C0,a15,a05+a25);
			cy5 = a25 - a05;
			Gx5 = cx5 - cx3;
			Gy5 = FMA(C0,cy4,cy3+cy5);
			*(pr++) = my_hypot(Gx2, Gy2) * C1;
			*(pr++) = my_hypot(Gx3, Gy3) * C1;
			*(pr++) = my_hypot(Gx4, Gy4) * C1;
			*(pr++) = my_hypot(Gx5, Gy5) * C1;
			px += 4;
		}
		switch(cols-i) { // catching remainder
			case 5:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				cx3 = cx4;
				cx4 = cx5;
				cy3 = cy4;
				cy4 = cy5;
				cx5 = FMA(C0,a15,a05+a25);
				cy5 = a25 - a05;
				Gx5 = cx5 - cx3;
				Gy5 = FMA(C0,cy4,cy3+cy5);
				*(pr++) = my_hypot(Gx5, Gy5) * C1;
				px++;
			case 4:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				cx3 = cx4;
				cx4 = cx5;
				cy3 = cy4;
				cy4 = cy5;
				cx5 = FMA(C0,a15,a05+a25);
				cy5 = a25 - a05;
				Gx5 = cx5 - cx3;
				Gy5 = FMA(C0,cy4,cy3+cy5);
				*(pr++) = my_hypot(Gx5, Gy5) * C1;
				px++;
			case 3:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				cx3 = cx4;
				cx4 = cx5;
				cy3 = cy4;
				cy4 = cy5;
				cx5 = FMA(C0,a15,a05+a25);
				cy5 = a25 - a05;
				Gx5 = cx5 - cx3;
				Gy5 = FMA(C0,cy4,cy3+cy5);
				*(pr++) = my_hypot(Gx5, Gy5) * C1;
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
	float s2 = FMA(a,a,b * b);
	float x = s2 * -0.5f;
	long i  = * ( long * ) &s2;
	i  = 0x5f375a86 - ( i >> 1 );
	float y  = * ( float * ) &i;
	y = y * FMA(x, y*y, 1.5f); // 1st Newton iteration
	y = y * FMA(x, y*y, 1.5f); // 2nd iteration, this can be removed
	return s2 * y;
}
