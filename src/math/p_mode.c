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
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_mode_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    unsigned int occurrence_count = 0;
    unsigned int max_occurrence_count = 0;
    unsigned int i = 1;
    float mode_value = 0.0f;
    float *sorted_a = (float*) p_malloc(team, sizeof(float) * n);
    p_sort_f32(a, sorted_a, n, p, team);

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

    p_free(sorted_a);
}
