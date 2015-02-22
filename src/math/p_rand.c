/* A tiny random number generator is needed */


#include "pal_base.h"
#include "../base/pal_base_private.h"

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
    tinymt32_init(&__pal_global.random, seed);
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
    return (int) tinymt32_generate_uint32(&__pal_global.random);
}
