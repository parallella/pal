#include <pal.h>

/**
 * RGB to Grayscale converter
 *
 * Each channel is replaced with the grayscale value.
 * The grayscale value is computed as 0.299*R + 0.587*G + 0.114*B
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols' x 3 bytes (1 byte/channel)
 *
 * @param r     Pointer to output image
 *
 * @param rows  Number of rows in input image
 *
 * @param cols  Number of columns in input image
 *
 */

void p_rgb2grayscale_f32(const float *x, float *r, int rows, int cols)
{
	int i;
	const float* px = x;
	float *pr = r;
	float E;

	for (i=0; i < rows*cols; i++)
	{
		E = (*px++) * 0.299f;
		E = __builtin_fmaf(0.587f, *px++, E);
		E = __builtin_fmaf(0.114f, *px++, E); 
		*pr++ = E;	
		*pr++ = E;	
		*pr++ = E;	
	}
}
