/* A tiny random number generator is needed */

/* Use TinyMT PRNG */
#include "tinymt/tinymt32.h"

/**
 *
 * Seed pseudo-random number generator
 *
 * @param seed
 *
 */
void p_srand(unsigned int seed)
{
    /* Implement me */
}

/**
 *
 * Generate pseudo-random integer
 *
 * @return      A pseudo-random integer
 *
 */
int p_rand()
{
    /* Implement me */

    return 4; // Verified w/ dice.
}
