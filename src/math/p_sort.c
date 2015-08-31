#include <pal.h>

#define SIFTDOWN_FUNC(name, type) \
	static void name (type *a, const uint32_t start, const uint32_t end) \
    { uint32_t root=start; /* outside of for loop for c compatibility*/ \
        for (; root * 2 + 1 <= end; ) { \
			uint32_t child = root * 2 + 1; \
			uint32_t swap = root; \
			\
			if (a[swap] < a[child]) \
				swap = child; \
			\
			if ((child + 1) <= end && a[swap] < a[child + 1]) \
				swap = child + 1; \
			\
			if (swap != root) { \
				type tmp = a[root]; \
				a[root] = a[swap]; \
				a[swap] = tmp; \
				root = swap; \
			} else \
				break; /* return, but that feels weird */ \
		} \
	}

SIFTDOWN_FUNC(_sift_down_f32, float);
SIFTDOWN_FUNC(_sift_down_u32, uint32_t);

#define SORT_FUNC(name, type, siftfunc) \
	static void name (const type *a, type *c, const int n) \
	{ \
		if (n < 1) \
			return; \
		\
        uint32_t count = (uint32_t) n; \
        uint32_t i = 0; /* outside of for loop for c compatibilty */\
		\
		/* copy to out */ \
        for (; i < count; i++) \
			c[i] = a[i]; \
		\
		/* heapify */ \
		uint32_t start = (count - 2) / 2; \
		\
		while (1) { \
			siftfunc(c, start, count - 1); \
			if (start > 0) \
				start--; \
			else \
				break; \
		} \
		\
		/* sort */ \
        {   \
        uint32_t end = count-1; /* outside of for loop for c compatibility */ \
        for (; end > 0;) { \
			type tmp = c[end]; \
			c[end] = c[0]; \
			c[0] = tmp; \
			end--; \
			siftfunc(c, 0, end); \
		} \
        } \
	}

SORT_FUNC(_heapsort_f32, float, _sift_down_f32);
SORT_FUNC(_heapsort_u32, uint32_t, _sift_down_u32);

/**
 *
 * Sorts an array of float values using heapsort
 *
 * @param a     Pointer to input vector
 * @param c     Pointer to result vector
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void p_sort_f32(const float *a, float *c, int n)
{
	_heapsort_f32(a, c, n);
}

/**
 *
 * Sorts an array of uint32_t values using heapsort
 *
 * @param a     Pointer to input vector
 * @param c     Pointer to result vector
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void p_sort_u32(const uint32_t *a, uint32_t *c, int n)
{
	_heapsort_u32(a, c, n);
}
