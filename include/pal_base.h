#pragma once

#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <stdbool.h>
#include <signal.h>

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
#define P_PROP_ROWBASE 6
#define P_PROP_COLBASE 7
#define P_PROP_PLANEBASE 8
#define P_PROP_CHIPROWS 9
#define P_PROP_CHIPCOLS 10
#define P_PROP_SIMD 11    // 1 if device supports SIMD
#define P_PROP_MEMSIZE 12 // Memory size per node
#define P_PROP_MEMBASE 13
#define P_PROP_VERSION 14
#define P_PROP_MEMARCH 15
#define P_PROP_WHOAMI 16

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
typedef p_ref_t p_memptr_t;

typedef struct {
    p_ref_t ref;
    size_t size;
    p_team_t team;
    int rank;
    p_dev_t dev;
    void *ops;
} p_mem_t;

typedef struct {
    p_team_t team;
    int mutex;
} p_mutex_t;


/*
 ***********************************************************************
 * STATIC INITIALIZERS
 ***********************************************************************
 */

/*#define _P_MAX_MAGIC 4095*/
/*#define _P_MIN_MAGIC 2048*/

#define _P_TEAM_DEFAULT 2048
#define P_TEAM_DEFAULT ((p_team_t *) ((intptr_t) -_P_TEAM_DEFAULT))

#define P_MUTEX_INITIALIZER { P_TEAM_DEFAULT, 0 }


/*
 ***********************************************************************
 * PROGRAM FLOW
 ***********************************************************************
 */

#define P_RUN_MAX_ARGS 16
typedef struct p_arg_t {
    void *ptr;
    size_t size;
    /* Needed for calling convention: Args are passed on stack, or as pointers,
     * depending on type and size. */
    bool is_primitive;
} p_arg_t;

#define P_RUN_NONBLOCK 0x800 /* == O_NONBLOCK */
#define P_RUN_PREPARE   (1 << 0)
#define P_RUN_PREPARED  (1 << 1)


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

/*Map team member, not all devices will support this*/
void *p_map_member(p_team_t team, int member, unsigned long off,
                   unsigned long size);

/* Map device address space into host address space */
p_mem_t p_map(p_dev_t dev, unsigned long address, unsigned long size);

/*Unmap memory region*/
int p_unmap(p_team_t team, p_mem_t *mem);

/*Close a team of processors*/
int p_close(p_team_t team);

/* Loads a program from the file system into memory */
p_prog_t p_load(p_dev_t dev, const char *file, int flags);

/* Run a program on N processors */
int p_run(p_prog_t prog, const char *function, p_team_t team,
          int start, int count, int nargs, const p_arg_t *args, int flags);

/* Send signal to team */
int p_kill(p_team_t team, int start, int count, int signal);

/*Execution barrier*/
int p_barrier(p_team_t team);

/* Wait for a team */
int p_wait(p_team_t team);

/*Create a local memory object*/
p_mem_t p_malloc(p_team_t team, size_t size);

/*Create a remote memory object*/
p_mem_t p_rmalloc(p_team_t team, int pid, size_t size);

/*Free allocated memory */
int p_free(p_mem_t *mem);

/*Memory fence*/
void p_fence(void);

/*Flushes the read and write paths to a specific memory object*/
int p_flush(p_mem_t *mem);

/*Query a property of a device*/
/*need it for mem, team, prog as well?*/
int p_query(p_dev_t dev, int property);

/*
 ***********************************************************************
 * MOVING BITS AROUND
 ***********************************************************************
 */

/*Writes to a global memory address from a local address*/
ssize_t p_write(p_mem_t *mem, const void *src, off_t offset, size_t nb, int flags);

/*Reads from a global memory address */
ssize_t p_read(p_mem_t *mem, void *dst, off_t offset, size_t nb, int flags);

/*Broadcasts an array based to a list of destination pointers*/
ssize_t p_broadcast(p_mem_t **mlist[], int mcount, void *src, size_t nb,
                    int flags);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_scatter(p_mem_t **mlist[], int mcount, void *suf, size_t scount,
                  int disp[], int flags);

/*Scatters data from a local array to a list of remote memory objects*/
ssize_t p_gather(p_mem_t **mlist[], int mcount, void *dbuf, size_t dcount,
                 int disp[], int flags);

/*Specialized low level shared memory memcpy interface (non-blocking)*/
ssize_t p_memcpy(void *dst, const void *src, size_t nb, int flags);

/*
 ***********************************************************************
 * SYNCHRONIZATION PRIMITIVES (SHARED MEMORY)
 ***********************************************************************
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
int p_mutex_init(p_mutex_t *mutex, p_team_t team);

/*Lock a mutex (try until fail)*/
int p_mutex_lock(p_mutex_t *mutex);

/*Try locking a mutex once*/
int p_mutex_trylock(p_mutex_t *mutex);

/*Unlock a mutex*/
int p_mutex_unlock(p_mutex_t *mutex);

/*atomic fetch and add*/
uint8_t p_atomic_add_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_add_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_add_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_add_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_add_i8(int8_t *atom, int8_t n);
int16_t p_atomic_add_i16(int16_t *atom, int16_t n);
int32_t p_atomic_add_i32(int32_t *atom, int32_t n);
int64_t p_atomic_add_i64(int64_t *atom, int64_t n);

/*atomic fetch and subtract*/
uint8_t p_atomic_sub_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_sub_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_sub_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_sub_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_sub_i8(int8_t *atom, int8_t n);
int16_t p_atomic_sub_i16(int16_t *atom, int16_t n);
int32_t p_atomic_sub_i32(int32_t *atom, int32_t n);
int64_t p_atomic_sub_i64(int64_t *atom, int64_t n);

/*atomic fetch and bitwise 'and'*/
uint8_t p_atomic_and_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_and_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_and_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_and_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_and_i8(int8_t *atom, int8_t n);
int16_t p_atomic_and_i16(int16_t *atom, int16_t n);
int32_t p_atomic_and_i32(int32_t *atom, int32_t n);
int64_t p_atomic_and_i64(int64_t *atom, int64_t n);

/*atomic fetch and bitwise 'xor'*/
uint8_t p_atomic_xor_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_xor_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_xor_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_xor_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_xor_i8(int8_t *atom, int8_t n);
int16_t p_atomic_xor_i16(int16_t *atom, int16_t n);
int32_t p_atomic_xor_i32(int32_t *atom, int32_t n);
int64_t p_atomic_xor_i64(int64_t *atom, int64_t n);

/*atomic fetch and bitwise 'or'*/
uint8_t p_atomic_or_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_or_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_or_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_or_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_or_i8(int8_t *atom, int8_t n);
int16_t p_atomic_or_i16(int16_t *atom, int16_t n);
int32_t p_atomic_or_i32(int32_t *atom, int32_t n);
int64_t p_atomic_or_i64(int64_t *atom, int64_t n);

/*atomic swap*/
uint8_t p_atomic_swap_u8(uint8_t *atom, uint8_t n);
uint16_t p_atomic_swap_u16(uint16_t *atom, uint16_t n);
uint32_t p_atomic_swap_u32(uint32_t *atom, uint32_t n);
uint64_t p_atomic_swap_u64(uint64_t *atom, uint64_t n);
int8_t p_atomic_swap_i8(int8_t *atom, int8_t n);
int16_t p_atomic_swap_i16(int16_t *atom, int16_t n);
int32_t p_atomic_swap_i32(int32_t *atom, int32_t n);
int64_t p_atomic_swap_i64(int64_t *atom, int64_t n);

/*atomic compare and swap*/
uint8_t p_atomic_compswap_u8(uint8_t *atom, uint8_t oldval, uint8_t newval);
uint16_t p_atomic_compswap_u16(uint16_t *atom, uint16_t oldval, uint16_t newval);
uint32_t p_atomic_compswap_u32(uint32_t *atom, uint32_t oldval, uint32_t newval);
uint64_t p_atomic_compswap_u64(uint64_t *atom, uint64_t oldval, uint64_t newval);
int8_t p_atomic_compswap_i8(int8_t *atom, int8_t oldval, int8_t newval);
int16_t p_atomic_compswap_i16(int16_t *atom, int16_t oldval, int16_t newval);
int32_t p_atomic_compswap_i32(int32_t *atom, int32_t oldval, int32_t newval);
int64_t p_atomic_compswap_i64(int64_t *atom, int64_t oldval, int64_t newval);

/** @todo Add description */
int p_getaddr(p_mem_t mem);
/** @todo Add description */
int p_getsymbol(p_prog_t prog, char *name, p_symbol_t symbol);


/*
 ***********************************************************************
 * Error handling
 ***********************************************************************
 */

#define P_MAX_ERROR 2047

static inline int p_error(p_ref_t ref)
{
    if ((uintptr_t) ref >= (uintptr_t) - P_MAX_ERROR)
        return (int) ((intptr_t) ref);

    return 0;
}

static inline int p_mem_error(p_mem_t *mem)
{
    return p_error(mem->ref);
}

/*
 ***********************************************************************
 * Coordinates and ranks
 ***********************************************************************
 */

/* Flags */
#define P_COORDS_ABSOLUTE   0
#define P_COORDS_RELATIVE   1
#define P_COORDS_WRAP_ROW   2
#define P_COORDS_WRAP_COL   4
#define P_COORDS_WRAP_PLANE 8

typedef union {
    int id;
    struct {
        int col;
        int row;
        int plane;
    };
} p_coords_t;

typedef enum {
    P_TOPOLOGY_FLAT,
    P_TOPOLOGY_2D,
    P_TOPOLOGY_3D,
    /* ... */
} p_topology_t;


int p_team_rank(p_team_t team);
int p_coords_to_rank(p_team_t team, const p_coords_t *coords, int flags);
int p_rel_coords_to_rank(p_team_t team, int rank, const p_coords_t *coords,
                         int flags);
int p_rank_to_coords(p_team_t team, int rank, p_coords_t *coords, int flags);
