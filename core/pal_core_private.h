
struct p_dev {
    int property[32];   
};

struct p_team {    
    p_dev_t *dev;         
    int    size;
    u32    proc_bitmap[256]; //holds one bit per processor
    //A lot less efficient to make it generic rather than block
    //based
};

struct p_mem {
    p_dev_t *dev;
    size_t size;
    void* memptr;
    
};

struct p_program {
    struct p_dev* dev;
    size_t progsize;
    char* program;
};

struct p_symbol {

};

struct p_event {

};

struct p_atom {

};

struct p_mutex {

};
