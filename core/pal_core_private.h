
struct p_dev {
    int property[32];   
};

struct p_team {    
    p_dev_t *dev;      //pointer to the parent device structure
    int      size;     //number of member nodes    
    int     *teamptr;  //list of addresses, one per node (int)
    int     *statptr;  //list of status regs, one per node (char)   
};

struct p_mem {
    p_dev_t *dev;         
    size_t   size;     //size of memory buffer
    void    *memptr;   //pointer to allocated memory
};

struct p_program {
    p_dev_t *dev;
    char    *name;
};

struct p_symbol {

};

struct p_event {

};

struct p_atom_u32 {
    u32 mutex;   //resource mutex
    u32 var;     //atomic variable
};

struct p_mutex {
    u32 mutex;   //mutex
};
