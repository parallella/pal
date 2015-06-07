#include <pal.h>

/**
 *
 * Finds the k-th value of input vector 'a' as if it were sorted.
 * Note that this will touch the input vector and swap elements.
 * Make a copy before calling if you want to keep it unchanged.
 * The time complexity is linear over the vector size. O(n).
 *
 * @param v     Pointer to input vector
 *
 * @param e     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @param k     Index of the element to find.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with
 *
 * @return      None
 *
 */

#define swap(a, b) {float c = (a); (a) = (b); (b) = c;}

void p_find_kth_f32(float* v, float* e, int n, int k, int p, p_team_t team) {
    if (n == 1 && k == 0) {
        *e = v[0];
        return;
    }

    int m = (n + 4)/5;
    float pivot;
    {
        float meds[m];
        for (int i = 0; i < m; i++) {
            if (5*i + 4 < n) {
                float* w = v + 5*i;
                for (int j0 = 0; j0 < 3; j0++) {
                    int jmin = j0;
                    for (int j = j0+1; j < 5; j++)
                        if (w[j] < w[jmin])
                            jmin = j;
                    swap(w[j0], w[jmin]);
                }
                meds[i] = w[2];
            } else {
                meds[i] = v[5*i];
            }
        }

        p_find_kth_f32(meds, &pivot, m, m/2, p, team);
    }

    for (int i = 0; i < n; i++) {
        if (v[i] == pivot) {
            swap(v[i], v[n-1]);
            break;
        }
    }

    int store = 0;
    for (int i = 0; i < n-1; i++)
        if (v[i] < pivot)
            swap(v[i], v[store++]);

    swap(v[store], v[n-1]);

    if (store == k)
        *e = pivot;
    else if (store > k)
        p_find_kth_f32(v, e, store, k, p, team);
    else
        p_find_kth_f32(v+store+1, e, n-store-1, k-store-1, p, team);
}

