#include <pal.h>

static void swap(float *lhs, float *rhs)
{
    float tmp = *lhs;
    *lhs = *rhs;
    *rhs = tmp;
}

static unsigned int median_partition(float *a, unsigned int left, unsigned int right, unsigned int pivot_index)
{
    unsigned int store_index = left;
    unsigned int i = left;

    float pivot = a[pivot_index];
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
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_median_f32(float *a, float *c, size_t n, int p, p_team_t team)
{
    unsigned int left = 0;
    unsigned int median_index = (n - 1) >> 1;
    float median_value = 0.0f;
    float search_a[n];
    
    p_memcpy(search_a, a, sizeof(float) * n, P_FLAG_DEFAULT);
    
    for (; median_index <= (n >> 1); ++median_index) {
        unsigned int right = n - 1;
        unsigned int pivot_index = 0;
        
        do {
            pivot_index = median_partition(search_a, left, right, (left + right) >> 1);

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
