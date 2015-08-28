#include <pal.h>

#define SORT(a,b) \
do { \
	float d; \
	if (a < b) { \
		d = a; \
		a = b; \
		b = d; \
	} \
} while(0)

static __inline __attribute__((__always_inline__))
float my_median(
	float v0, float v1, float v2,
	float v3, float v4, float v5,
	float v6, float v7, float v8);

/*
 * A median 3x3 filter.
 *
 * @param x     Pointer to input image, a 2D array of size 'rows' x 'cols'
 * @param r     Pointer to output image, a 2D array pf size 'rows-2' x 'cols-2'
 * @param rows  Number of rows in input image
 * @param cols  Number of columns in input image
 * @return      None
 */

void p_median3x3_f32(const float *x, float *r, int rows, int cols)
{
	float v0,v1,v2,v3,v4,v5,v6,v7,v8;
	const float *px = x;
	float *pr = r;
	int i, j;

	for (i = 0; i < rows - 2; i++) {
		v3 = *(px);
		v4 = *(px + cols);
		v5 = *(px + 2*cols);
		SORT(v4,v5);
		SORT(v3,v4);
		SORT(v4,v5);
		v6 = *(px + 1);
		v7 = *(px + cols + 1);
		v8 = *(px + 2*cols + 1);
		SORT(v7,v8);
		SORT(v6,v7);
		SORT(v7,v8);
		for (j = 0; j < cols - 2; j++) {
			v0 = v3;
			v1 = v4;
			v2 = v5;
			v3 = v6;
			v4 = v7;
			v5 = v8;
			v6 = *(px + 2);
			v7 = *(px + cols + 2);
			v8 = *(px + 2*cols + 2);
			SORT(v7,v8);
			SORT(v6,v7);
			SORT(v7,v8);

			*(pr++) = my_median(v0,v1,v2,v3,v4,v5,v6,v7,v8);
			px++;
		}
		px += 2;
	}
}

#define SORT_HI(a,b) { if (b > a) b = a; }
#define SORT_LO(a,b) { if (a < b) a = b; }

/*
 * A specialized median 3x3 filter.
 *
 * The routine is inlined for performance and requires that the v0-v2,
 * v3-v5, and v6-v8 triplets be ordered from largest to smallest. This
 * improves performance while sweeping through columns of values.
 * Macros handle the swapping of values and do not swap values which
 * won't be used again.
 *
 * @param v0    The largest value in the triplet v0-v2
 * @param v1    The median value in the triplet v0-v2
 * @param v2    The smallest value in the triplet v0-v2
 * @param v3    The largest value in the triplet v3-v5
 * @param v4    The median value in the triplet v3-v5
 * @param v5    The smallest value in the triplet v3-v5
 * @param v6    The largest value in the triplet v6-v8
 * @param v7    The median value in the triplet v6-v8
 * @param v8    The smallest value in the triplet v6-v8
 * @return      The median value of the set v0-v8
 */

static __inline __attribute((__always_inline__))
float my_median(
	float v0, float v1, float v2,
	float v3, float v4, float v5,
	float v6, float v7, float v8)
{
	SORT_HI(v0,v3);
	SORT_LO(v5,v8);
	SORT(v4,v7);
	SORT_HI(v3,v6);
	SORT_HI(v1,v4);
	SORT_LO(v2,v5);
	SORT_LO(v4,v7);
	SORT(v4,v2);
	SORT_HI(v6,v4);
	SORT_LO(v4,v2);
	return v4;
}

