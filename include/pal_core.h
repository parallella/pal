#ifndef _PAL_CORE_H_
#define _PAL_CORE_H_

#include <stdlib.h>
#include <stdint.h>


/*
 ***********************************************************************
 * DEVICE TYPE
 ***********************************************************************
 */

#define EPIPHANY  0x01
#define GRID      0x02
#define SMP       0x03
#define FPGA      0x04
#define GPU       0x05
#define DEMO      0x06

/*
 ***********************************************************************
 * FLAGS
 ***********************************************************************
 */
#define ASYNC      0x01
#define METAL      0x02
#define LINUX      0x04
#define SOURCE     0x08
#define BIN        0x10

#define DEFAULT    0
#define STANDARD   0
#define SUCCESS    0
#define ERROR     -1

/*
 ***********************************************************************
 *DEVICE QUERY PARAMETERS
 ***********************************************************************
 */

#define TYPE           0
#define NODES          1
#define TOPOLOGY       2
#define ROWS           3
#define COLS           4
#define PLANES         5
#define CHIPROWS       6
#define CHIPCOLS       7
#define SIMD           8
#define MEMSIZE        9
#define MEMBASE        10
#define VERSION        11
#define MEMARCH        12
#define WHOAMI         13
/*
 ***********************************************************************
 * OPAQUE OBJECT TYPES 
 ***********************************************************************
 */

typedef struct p_dev_table p_dev_table_t;
typedef struct p_team_table p_team_table_t;
typedef struct p_program_table p_program_table_t;
typedef struct p_mem_table p_mem_table_t;
typedef struct p_dev p_dev_t;
typedef struct p_team p_team_t;
typedef struct p_program p_program_t;
typedef struct p_symbol p_symbol_t;
typedef struct p_event p_event_t;
typedef struct p_mem p_mem_t;
typedef struct p_memptr p_memptr_t;
typedef struct p_atom p_atom_t;
typedef struct p_mutex p_mutex_t;
typedef struct p_mutex_attr p_mutex_attr_t;

/*
 ***********************************************************************
 * PROGRAM FLOW
 ***********************************************************************
 */

/*Initialize device run time*/
int p_init(int type, int flags);

/*Finalize device run time*/
int p_finalize(int dev);

/*Open a team of processors*/
int p_open(int dev, int start, int count);

/*Add team members*/
int p_append(int team, int start, int count);

/*Remove team members*/
int p_remove(int team, int start, int count);

/*Close a team of processors*/
int p_close(int team); 

/*Loads a program from the file system into memory */
int p_load(int dev, char *file, char *function, int flags);

/*Run a program on N processors*/
int p_run(int prog, int team, int start, int count, 
	  int nargs, void *args[], int flags);

/*Execution barrier*/
int p_barrier(p_team_t team);

/* Wait for a team */
int p_wait(p_team_t team);

/*Create a local memory object*/
int p_malloc(int team, size_t size);

/*Create a remote memory object*/
int p_rmalloc(int team, int pid, size_t size);

/*Free allocated memory */
int p_free(int mem);

/*Memory fence*/
int p_fence(int mem);

/*Flushes the read and write paths to a specific memory object*/
ssize_t p_flush(int mem);

/*Query a property of a device*/
/*need it for mem, team, prog as well?*/
int p_devquery(int dev, int property);

/*
 ***********************************************************************
 * MOVING BITS AROUND
 ***********************************************************************
 */

/*Writes to a global memory address from a local address*/
ssize_t p_write(int mem, const void *src, size_t nb, int flags);

/*Reads from a global memory address */
ssize_t p_read(int mem, void *dst, size_t nb, int flags);    

/*Broadcasts an array based to a list of destination pointers*/
ssize_t p_broadcast(int *mlist[], int mcount, void *src, size_t nb, int flags);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_scatter(int *mlist[], int mcount, 
		  void *suf, size_t scount,
		  int disp[], int flags); 

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_gather(int *mlist[], int mcount, 
		 void *dbuf, size_t dcount,
		 int disp[], int flags); 

/*Specialized low level shared memory memcpy interface (non-blocking)*/
ssize_t p_memcpy(void *dst, void *src, size_t nb, int flags);

/*
 ***********************************************************************
 * SYNCHRONIZATION PRIMITIVES (SHARED MEMORY)
 ***********************************************************************
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
int p_mutex_init(p_mutex_t *mp);

/*Lock a mutex (try until fail)*/
int p_mutex_lock(p_mutex_t *mp);

/*Try locking a mutex once*/
int p_mutex_trylock(p_mutex_t *mp);

/*Unlock a mutex*/
int p_mutex_unlock(p_mutex_t *mp);

/*Destroy a mutex*/
int p_mutex_destroy(p_mutex_t *mp);

/*atomic fetch and add*/
int p_atomic_add_u32(p_atom_t *atom, uint32_t n);

/*atomic fetch and subtract*/
int p_atomic_sub_u32(p_atom_t *atom, uint32_t n);

/*atomic fetch and logical 'and'*/
int p_atomic_and_u32(p_atom_t *atom, uint32_t n);

/*atomic fetch and logical 'xor'*/
int p_atomic_xor_u32(p_atom_t *atom, uint32_t n);

/*atomic fetch and logical 'or'*/
int p_atomic_or_u32(p_atom_t *atom, uint32_t n);

/*atomic fetch and logical 'nand'*/
int p_atomic_nand_u32(p_atom_t *atom, uint32_t n);

/*atomic swap*/
int p_atomic_swap_u32(p_atom_t *atom, uint32_t *input);

/*atomic compare and swap*/
int p_atomic_compswap_u32(p_atom_t *atom, uint32_t *input, uint32_t expected);


/*
 ***********************************************************************
 * Global system variables (will be replaced with O/S real time
 ***********************************************************************
 */

struct p_dev_table {
    p_dev_t *devptr[16];
    int size;
};
struct p_team_table {
    p_team_t *teamptr[16];
    int size;
};
struct p_program_table {
    p_program_t *progptr[16];
    int size;
};
struct p_mem_table {
    p_mem_t *memptr[16];
    int size;
};

struct p_dev_table p_dev_table_global;
struct p_team_table p_team_table_global;
struct p_program_table p_program_table_global;
struct p_mem_table p_mem_table_global;
