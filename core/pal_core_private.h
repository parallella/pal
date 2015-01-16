#include "pal_core.h"
#define N 4096

struct pal_dev {
    int properties[32];   
};

struct pal_team {    
    struct pal_dev* dev;         
    u32    proc_bitmap[N/32]; //holds one bit per processor
    //A lot less efficient to make it generic rather than block
    //based
};

struct pal_mem {
    struct pal_dev* dev;
    size_t memsize;
    void* mem;
    
};

struct pal_program {
    struct pal_dev* dev;
    size_t progsize;
    void* program;
};

struct pal_symbol {

};

struct pal_event {

};

struct pal_atom {

};

struct pal_mutex {

};
