#include <pal.h>
#include <time.h>

static void swap(float *lhs, float *rhs)
{
    if (lhs != rhs) {
        *lhs += *rhs;
        *rhs = *lhs - *rhs;
        *lhs = *lhs - *rhs;
    }
}

static unsigned int median_partition(float *a, unsigned int left, unsigned int right, unsigned int pivot_index)
{
    unsigned int store_index = left;
    unsigned int i = left;

    float pivot = a[pivot_index];
    swap(&a[pivot_index], &a[right - 1]);

    for (; i < right - 1; ++i) {
        if (a[i] < pivot)
            swap(&a[i], &a[store_index++]);
    }

    swap(&a[store_index], &a[right - 1]);

    return store_index;
}

static float median_select(float *a, int n, int mean, unsigned int left, unsigned int right) {
    if (left == right)
        return a[left];

    unsigned int pivot_index = left + p_rand() % (right - left);
    pivot_index = median_partition(a, left, right, pivot_index);

    if (n == pivot_index) {
        if (mean)
            return (a[pivot_index] + median_select(a, n + 1, !mean, pivot_index + 1, right)) / 2.0f;
        else
            return a[pivot_index];
    } else if (pivot_index > n) {
        return median_select(a, n, mean, left, pivot_index);
    } else {
        return median_select(a, n, mean, pivot_index + 1, right);
    }
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

void p_median_f32(float *a, float *c, int n, int p, p_team_t team)
{
    float *search_a = (float*) p_malloc(team, sizeof(float) * n);
    p_memcpy(search_a, a, sizeof(float) * n, P_FLAG_DEFAULT);
    p_srand(time(0));
    *c = median_select(search_a, (n - 1) / 2, !(n % 2), 0, n);
    p_free(search_a);
}
