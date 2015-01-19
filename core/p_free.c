#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_free (void *obj){
    printf("Running p_free(obj)\n");
    return(0);
}
