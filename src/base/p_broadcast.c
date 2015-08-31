#include <pal.h>
#include <stdio.h>
#include <stddef.h>
#include "pal_base.h"
#include "pal_base_private.h"

ssize_t p_scatter(p_mem_t *mlist[], int mcount, void *suf, size_t scount,
                  int disp[], int flags)
{
    UNUSED(mlist);
    UNUSED(mcount);
    UNUSED(suf);
    UNUSED(scount);
    UNUSED(disp);
    UNUSED(flags);
    printf("Running p_broadcast()\n");
    return (0);
}
