#include <pal.h>

#define C0 (0.0751136080f)
#define C1 (0.1238414032f)
#define CS (1.6487212707f) //(C1/C0)
#define FMA(a,b,c) __builtin_fmaf(a,b,c)

/**
 * A 3x3 gauss smoothing filter with the following convolution matrix
 *
 *     | 0.0751136080 0.1238414032 0.0751136080 |
 * M = | 0.1238414032 0.2041799556 0.1238414032 |
 *     | 0.0751136080 0.1238414032 0.0751136080 |
 *
 * Notes: cols and rows may be any size
 *  Coefficients calculated with the Gaussian 2D distribution equation:
 *    e^(-(px^2+py^2)/(2*S^2)) and then normalized
 *  Sigma (S) is 1, px and py are pixel offsets.
 *  A naive algorithm uses 14 or 17 operations per pixel.
 *  The optimized algorithm below uses 8 operations per pixel.
 *
 * @param x    Pointer to input image, a 2D array of size 'rows' x 'cols'
 *
 * @param r    Pointer to output image
 *
 * @param rows Number of rows in input image
 *
 * @param cols Number of columns in input image
 *
 * @return     None
 *
 */

void p_gauss3x3_f32(const float * x, float * r, int rows, int cols)
{
	int i, j;
	float a02, a03, a04, a05;
	float a12, a13, a14, a15;
	float a22, a23, a24, a25;
	float c0, c1, c2, c3, c4, c5;

	const float *px = x;
	float *pr = r + cols + 1;

	for (j = 0; j < (rows - 2); j++) {
		// Unroll 4x is smaller code than 5x and maybe faster. The prefetch and loop
		// block may be removed to possibly reduce code size with reduced performance
		a04 = px[0];
		a05 = px[1];
		a14 = px[cols+0];
		a15 = px[cols+1];
		a24 = px[2*cols+0];
		a25 = px[2*cols+1];
		c4 = FMA(C1,a14,FMA(C0,a04,C0*a24));
		c5 = FMA(C1,a15,FMA(C0,a05,C0*a25));
		for (i = 0; i < (cols - 5); i += 4) {
			a02 = px[2];
			a03 = px[3];
			a04 = px[4];
			a05 = px[5];
			a12 = px[cols+2];
			a13 = px[cols+3];
			a14 = px[cols+4];
			a15 = px[cols+5];
			a22 = px[2*cols+2];
			a23 = px[2*cols+3];
			a24 = px[2*cols+4];
			a25 = px[2*cols+5];
			c0 = c4;
			c1 = c5;
			c2 = FMA(C1,a12,FMA(C0,a02,C0*a22));
			c3 = FMA(C1,a13,FMA(C0,a03,C0*a23));
			c4 = FMA(C1,a14,FMA(C0,a04,C0*a24));
			c5 = FMA(C1,a15,FMA(C0,a05,C0*a25));
			*(pr++) = FMA(CS,c1,c0+c2);
			*(pr++) = FMA(CS,c2,c1+c3);
			*(pr++) = FMA(CS,c3,c2+c4);
			*(pr++) = FMA(CS,c4,c3+c5);
			px += 4;
		}
		// catching remainder
		switch(cols-i) {
			case 5:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				c3 = c4;
				c4 = c5;
				c5 = FMA(C1,a15,FMA(C0,a05,C0*a25));
				*(pr++) = FMA(CS,c4,c3+c5);
				px++;
			case 4:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				c3 = c4;
				c4 = c5;
				c5 = FMA(C1,a15,FMA(C0,a05,C0*a25));
				*(pr++) = FMA(CS,c4,c3+c5);
				px++;
			case 3:
				a05 = px[2];
				a15 = px[cols+2];
				a25 = px[2*cols+2];
				c3 = c4;
				c4 = c5;
				c5 = FMA(C1,a15,FMA(C0,a05,C0*a25));
				*(pr++) = FMA(CS,c4,c3+c5);
				px++;
		}
		px += 2;
		pr += 2;
	}
	return;
}
