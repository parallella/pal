#include <string.h>
#include <pal.h>

static void swap(PTYPE *lhs, PTYPE *rhs)
{
    PTYPE tmp = *lhs;
    *lhs = *rhs;
    *rhs = tmp;
}

static unsigned int median_partition(PTYPE *a,
                                     unsigned int left,
                                     unsigned int right,
                                     unsigned int pivot_index)
{
    unsigned int store_index = left;
    unsigned int i = left;

    PTYPE pivot = a[pivot_index];
    swap(&a[pivot_index], &a[right]);

    for (; i < right; ++i) {
        if (a[i] < pivot)
            swap(&a[i], &a[store_index++]);
    }

    swap(&a[store_index], &a[right]);

    return store_index;
}

/**
 *
 * Calculates the median value of input vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @return      None
 *
 */

void PSYM(p_median)(const PTYPE *a, PTYPE *c, int n)
{
    unsigned int left = 0;
    unsigned int median_index = (n - 1) >> 1;
    PTYPE median_value = 0.0f;
    PTYPE search_a[n];

    memcpy(search_a, a, sizeof(PTYPE) * n);

    for (; median_index <= (n >> 1); ++median_index) {
        unsigned int right = n - 1;
        unsigned int pivot_index = 0;

        do {
            pivot_index =
                median_partition(search_a, left, right, (left + right) >> 1);

            if (pivot_index > median_index) {
                right = pivot_index - 1;
            } else {
                left = pivot_index + 1;
            }
        } while (pivot_index != median_index);

        median_value += search_a[pivot_index];
    }

    *c = (n % 2 ? median_value : median_value * 0.5);
}
