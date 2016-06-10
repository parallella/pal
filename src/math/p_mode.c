#include <pal.h>

/**
 *
 * Calculates the mode value of input vector 'a'.
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

void PSYM(p_mode)(const PTYPE *a, PTYPE *c, int n)
{
    unsigned int occurrence_count = 0;
    unsigned int max_occurrence_count = 0;
    unsigned int i = 1;
    PTYPE mode_value = 0.0f;
    PTYPE *sorted_a = (PTYPE*) malloc(sizeof(PTYPE) * n);
    PSYM(p_sort)(a, sorted_a, n);

    for (; i < n; ++i) {
        ++occurrence_count;
        if (sorted_a[i] != sorted_a[i - 1]) {
            if (occurrence_count > max_occurrence_count) {
                max_occurrence_count = occurrence_count;
                mode_value = sorted_a[i - 1];
            }
            occurrence_count = 0;
        }
    }
    if (occurrence_count > max_occurrence_count) {
        max_occurrence_count = occurrence_count;
        mode_value = sorted_a[n - 1];
    }

    *c = mode_value;

    free(sorted_a);
}
