/**
 *
 * A memory fence. Ensures that all read/write operations to memory object
 * from the processor exeucting the p_fence call have completed.
 *
 * @param mem   Memory object desciptor (int)
 *
 * @return      Returns 0 if successful.
 *
 */
#include "pal_base.h"
#include "pal_private.h"

int p_fence(int mem)
{
    printf("Running p_fence(%d)\n", mem);
    return (0);
}
