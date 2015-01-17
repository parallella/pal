#include "pal_core.h"
#include "pal_core_private.h"
#include <stdio.h>
int p_query (void *obj, int property){
    
    int res;
    p_dev_t *dev;
    /*TODO, need to be clever with queries different object types*/
    dev = (p_dev_t *) obj;    
    res=dev->property[property];
    return (res);
}
