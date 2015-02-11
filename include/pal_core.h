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

typedef int64_t p_ref_t;
typedef p_ref_t p_dev_table_t;

typedef p_ref_t p_team_table_t;
typedef p_ref_t p_program_table_t;
typedef p_ref_t p_mem_table_t;
typedef p_ref_t p_dev_t;
typedef p_ref_t p_team_t;
typedef p_ref_t p_program_t;
typedef p_ref_t p_symbol_t;
typedef p_ref_t p_event_t;
typedef p_ref_t p_mem_t;
typedef p_ref_t p_memptr_t;
typedef p_ref_t p_atom_t;
typedef p_ref_t p_mutex_t;
typedef p_ref_t p_mutex_attr_t;

/*
 ***********************************************************************
 * PROGRAM FLOW
 ***********************************************************************
 */

/*Initialize device run time*/
p_dev_t p_init(p_dev_t *dev, int type, int flags);

/*Finalize device run time*/
p_ref_t p_finalize(p_dev_t *dev);

/*Open a team of processors*/
p_team_t p_open(p_dev_t *dev, int start, int count);

/*Add team members*/
p_team_t p_append(p_team_t *team, int start, int count);

/*Remove team members*/
p_team_t p_remove(p_team_t *team, int start, int count);

/*Close a team of processors*/
p_ref_t p_close(p_team_t *team);

/* Loads a program from the file system into memory */
p_prog_t p_load(p_dev_t *dev, char *file, char *function, int flags,
        p_prog_t* prog);

/* Run a program on N processors */
p_ref_t p_run(p_prog_t *prog, p_team_t *team, int start, int count,
        int nargs, void *args[], int flags);

/*Execution barrier*/
int p_barrier(p_team_t team);

/* Wait for a team */
int p_wait(p_team_t team);

/*Create a local memory object*/
p_mem_t p_malloc(p_team_t team, size_t size);

/*Create a remote memory object*/
p_mem_t p_rmalloc(p_team_t team, int pid, size_t size);

/*Free allocated memory */
int p_free(p_mem_t mem);

/*Memory fence*/
int p_fence(p_mem_t mem);

/*Flushes the read and write paths to a specific memory object*/
int p_flush(p_mem_t mem);

/*Query a property of a device*/
/*need it for mem, team, prog as well?*/
int p_query(p_dev_t dev, int property);

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
