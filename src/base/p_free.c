/**
 *
 * Frees up resources occupied by 'mem'.
 *
 * @param mem Memory object descriptor (int)
 *
 * @return    Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

int p_free(p_mem_t mem)
{
    printf("Running p_free(%ld)\n", mem);
    return (0);
}
