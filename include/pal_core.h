#ifndef _PAL_CORE_H_
#define _PAL_CORE_H_

#include <stdlib.h>
#include <stdint.h>

/*
 ***********************************************************************
 * SPECIFIC TYPES (LINUX INSPIRED)
 ***********************************************************************
 */

typedef unsigned long long u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

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

typedef struct p_atom p_atom_t;
typedef struct p_mutex p_mutex_t;
typedef struct p_mutex_attr p_mutex_attr_t;

/*
 ***********************************************************************
 * PROGRAM FLOW
 ***********************************************************************
 */

/*Device structure setup*/
int p_init(int type, int flags);

/*Query a property of a device*/
int p_query(int dev, int property);

/*Loads a program from the file system into memory */
int p_load(int dev, char *file, char *function);

/*Open a team of processors*/
int p_open(int dev, int start, int count);

/*Add team members*/
int p_append(int team, int start, int count);

/*Remove team members*/
int p_remove(int team, int start, int count);

/*Run a program on N processors*/
int p_run(int prog, int team, int start, int count, 
	  int argn, void *args[], int flags);

/*Close a team of processors*/
int p_close(int team); 

/*Execution barrier*/
int p_barrier(int team);

/*Local memory allocation*/
int p_malloc(int team, size_t size);

/*Global memory allocation*/
int p_gmalloc(int team, int n, size_t size);

/*Free allocated memory */
int p_free(int mem);

/*Finalize device run time*/
int p_finalize(int dev);

/*Memory fence*/
int p_fence(int mem);

/*
 ***********************************************************************
 * MOVING BITS AROUND
 ***********************************************************************
 */

/*Writes to a global memory address from a local address*/
ssize_t p_write(int mem, const void *src, size_t nbytes, int flags);

/*Reads from a global memory address */
ssize_t p_read(int mem, void *dst, size_t nbytes, int flags);    

/*Flushes the read and write paths to a specific memory object*/
ssize_t p_flush(int mem);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_scatter(int *mlist[], void *src, size_t nsrc, size_t ndst, int flags); 

/*Gathers data from a list of remote memory objects into a local array*/
ssize_t p_gather(int *mlist[], void *dst, size_t nsrc, size_t ndst, int flags); 

/*Broadcasts an array based to a list of destination pointers*/
ssize_t p_broadcast(int *mlist[], void *src, size_t nsrc, size_t ndst, int flags); 

/*Specialized low level shared memory memcpy interface (non-blocking)*/
ssize_t p_memcpy(void *dst, void *src, size_t nbytes, int flags);

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
int p_atomic_add_u32(p_atom_t *atom, u32 n);

/*atomic fetch and subtract*/
int p_atomic_sub_u32(p_atom_t *atom, u32 n);

/*atomic fetch and logical 'and'*/
int p_atomic_and_u32(p_atom_t *atom, u32 n);

/*atomic fetch and logical 'xor'*/
int p_atomic_xor_u32(p_atom_t *atom, u32 n);

/*atomic fetch and logical 'or'*/
int p_atomic_or_u32(p_atom_t *atom, u32 n);

/*atomic fetch and logical 'nand'*/
int p_atomic_nand_u32(p_atom_t *atom, u32 n);

/*atomic swap*/
int p_atomic_swap_u32(p_atom_t *atom, u32 *input);

/*atomic compare and swap*/
int p_atomic_compswap_u32(p_atom_t *atom, u32 *input, u32 expected);

#endif
