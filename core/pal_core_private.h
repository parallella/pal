
struct p_dev {
    int property[32];   
};

struct p_team {    
    p_dev_t *dev;      //pointer to the associated device structure
    int      size;     //number of member nodes    
    int     *teamptr;  //list of addresses, one per node (int)
    int     *statptr;  //list of status "regs". idle/working
};

//Too much overhead?
struct p_mem {
    p_dev_t *team     //pointer to the associated team structure
    size_t   size;    //size of memory buffer
    void    *memptr;  //pointer to allocated memory
    int      takeit;  //indicates that new data is ready (wraparound impl)
    int      gotit;   //indicates that data was read (wraparound impl)
    int      mutex;   //optional mutex to grab before reading/writing 'mem' 
    int      pilot;   //temp var used for flushing read/write path to 'mem'
};

struct p_program {
    p_dev_t *dev;      //pointer to the associated device structure        
    char    *name;     //executable file name
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
