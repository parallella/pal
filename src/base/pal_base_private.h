#include <errno.h>
#include <stdint.h>

// TEMPORARY HACK!!
struct p_dev
{
    int property[32];
};

struct p_team
{
    p_dev_t *dev;   // pointer to the associated device structure
    int size;       // number of member nodes
    p_team_t *team; // list of addresses, one per node (int)
    p_team_t *stat; // list of status "regs". idle/working
};

// Optimize later, focus on function...
struct p_mem
{
    p_team_t *team; // pointer to the associated team structure
    int mutex;      // optional mutex to grab before reading/writing 'mem'
    int takeit;     // indicates that new data is ready (wraparound impl)
    int gotit;      // indicates that data was read (wraparound impl)
    int pilot;      // temp var used for flushing read/write path to 'mem'
    size_t size;    // size of memory buffer
    void *mem;      // pointer to allocated memory
};

struct p_program
{
    p_dev_t *dev; // pointer to the associated device structure
    char *name;   // executable file name
};

struct p_symbol
{
};

struct p_event
{
};

struct p_atom_u32
{
    uint32_t mutex; // resource mutex
    uint32_t var;   // atomic variable
};

struct p_mutex
{
    uint32_t mutex; // mutex
};

/*
 ***********************************************************************
 * Global system variables (will be replaced with O/S real time
 ***********************************************************************
 */

struct p_dev_table
{
    p_dev_t *devptr[16];
    int size;
};
struct p_team_table
{
    p_team_t *teamptr[16];
    int size;
};
struct p_program_table
{
    p_prog_t *progptr[16];
    int size;
};
struct p_mem_table
{
    p_mem_t *memptr[16];
    int size;
};

struct p_dev_table p_dev_table_global;
struct p_team_table p_team_table_global;
struct p_program_table p_program_table_global;
struct p_mem_table p_mem_table_global;

