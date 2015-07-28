#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>

/*
 ***********************************************************************
 * DEVICE TYPE
 ***********************************************************************
 */

#define P_DEV_EPIPHANY 0x01
#define P_DEV_GRID 0x02
#define P_DEV_SMP 0x03
#define P_DEV_FPGA 0x04
#define P_DEV_GPU 0x05
#define P_DEV_DEMO 0x06

#define P_DEV_FIRST P_DEV_EPIPHANY
#define P_DEV_LAST P_DEV_DEMO

/*
 ***********************************************************************
 * FLAGS
 ***********************************************************************
 */
#define P_FLAG_DEFAULT 0
#define P_FLAG_ASYNC 0x01
#define P_FLAG_METAL 0x02
#define P_FLAG_LINUX 0x04
#define P_FLAG_SOURCE 0x08
#define P_FLAG_BIN 0x10

/*
 ***********************************************************************
 *DEVICE QUERY PARAMETERS
 ***********************************************************************
 */

#define P_PROP_TYPE 0     // Type of device
#define P_PROP_NODES 1    // Number of nodes on devices
#define P_PROP_TOPOLOGY 2 // E.g., 2 == 2D mesh
#define P_PROP_ROWS 3
#define P_PROP_COLS 4
#define P_PROP_PLANES 5
#define P_PROP_CHIPROWS 6
#define P_PROP_CHIPCOLS 7
#define P_PROP_SIMD 8     // 1 if device supports SIMD
#define P_PROP_MEMSIZE 9  // Memory size per node
#define P_PROP_MEMBASE 10
#define P_PROP_VERSION 11
#define P_PROP_MEMARCH 12
#define P_PROP_WHOAMI 13

/*
 ***********************************************************************
 * OPAQUE OBJECT TYPES
 ***********************************************************************
 */

typedef void * p_ref_t;
typedef p_ref_t p_dev_table_t;

typedef p_ref_t p_team_table_t;
typedef p_ref_t p_program_table_t;
typedef p_ref_t p_mem_table_t;
typedef p_ref_t p_dev_t;
typedef p_ref_t p_team_t;
typedef p_ref_t p_prog_t;
typedef p_ref_t p_symbol_t;
typedef p_ref_t p_event_t;
typedef p_ref_t p_mem_t;
typedef p_ref_t p_memptr_t;


typedef int p_atom_t;
typedef int p_mutex_t;

/*
 ***********************************************************************
 * PROGRAM FLOW
 ***********************************************************************
 */

/*Initialize device run time*/
p_dev_t p_init(int type, int flags);

/*Finalize device run time*/
int p_finalize(p_dev_t dev);

/*Open a team of processors*/
p_team_t p_open(p_dev_t dev, int start, int count);

/*Add team members*/
p_team_t p_append(p_team_t team, int start, int count);

/*Remove team members*/
p_team_t p_remove(p_team_t team, int start, int count);

/*Close a team of processors*/
int p_close(p_team_t team);

/* Loads a program from the file system into memory */
p_prog_t p_load(p_dev_t dev, const char *file, const char *function, int flags);

/* Run a program on N processors */
int p_run(p_prog_t prog, p_team_t team, int start, int count, int nargs,
              const char *args[], int flags);

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
ssize_t p_write(p_mem_t mem, const void *src, size_t nb, int flags);

/*Reads from a global memory address */
ssize_t p_read(p_mem_t mem, void *dst, off_t offset, size_t nb, int flags);

/*Broadcasts an array based to a list of destination pointers*/
ssize_t p_broadcast(p_mem_t *mlist[], int mcount, void *src, size_t nb,
                    int flags);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_scatter(p_mem_t *mlist[], int mcount, void *suf, size_t scount,
                  int disp[], int flags);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_gather(p_mem_t *mlist[], int mcount, void *dbuf, size_t dcount,
                 int disp[], int flags);

/*Specialized low level shared memory memcpy interface (non-blocking)*/
ssize_t p_memcpy(void *dst, const void *src, size_t nb, int flags);

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
int p_atomic_add_u32(p_atom_t atom, uint32_t n);

/*atomic fetch and subtract*/
int p_atomic_sub_u32(p_atom_t atom, uint32_t n);

/*atomic fetch and logical 'and'*/
int p_atomic_and_u32(p_atom_t atom, uint32_t n);

/*atomic fetch and logical 'xor'*/
int p_atomic_xor_u32(p_atom_t atom, uint32_t n);

/*atomic fetch and logical 'or'*/
int p_atomic_or_u32(p_atom_t atom, uint32_t n);

/*atomic fetch and logical 'nand'*/
int p_atomic_nand_u32(p_atom_t atom, uint32_t n);

/*atomic swap*/
int p_atomic_swap_u32(p_atom_t atom, const uint32_t *input);

/*atomic compare and swap*/
int p_atomic_compswap_u32(p_atom_t atom, uint32_t *input, uint32_t expected);

/** @todo Add description */
int p_getaddr(p_mem_t mem);
/** @todo Add description */
int p_getsymbol(p_prog_t prog, char *name, p_symbol_t symbol);


/*
 ***********************************************************************
 * Error handling
 ***********************************************************************
 */
int p_get_err(p_ref_t ref);
