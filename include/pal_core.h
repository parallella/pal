#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <err.h>
#include <stdio.h>
#include <stdlib.h>

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
 * FLAGS
 ***********************************************************************
 */
#define SMP       0x01
#define OPENCL    0x02
#define CLUSTER   0x04
#define EPIPHANY  0x08
#define FPGA      0x10

/*
 ***********************************************************************
 * BIT FIELD FLAGS
 ***********************************************************************
 */
#DEFINE ASYNC     0x01

/*
 *DEVICE QUERY PARAMETERS
 */

#define NAME           0
#define WHOAMI         1
#define TOPOLOGY       2
#define ALL            3
#define ORIGIN         4
#define ROWS           5 
#define SIMD_SIZE      6
#define CHIP_COLS      7
#define CHIP_ROWS      8
#define LOC_MEMSIZE    9
#define SH_MEMSIZE    10
/*
 ***********************************************************************
 * STRUCTURES
 ***********************************************************************
 */

struct p_dev;
struct p_team;
struct p_program;
struct p_symbol;
struct p_event;
struct p_mem;
struct p_memptr;
struct p_atom;
struct p_mutex;
struct p_mutex_attr;

typedef struct p_dev* p_dev_t;
typedef struct p_team* p_team_t;
typedef struct p_program* p_program_t;
typedef struct p_symbol* p_symbol_t;
typedef struct p_event* p_event_t;
typedef struct p_mem* p_mem_t;
typedef struct p_memptr* p_memptr_t;
typedef struct p_atom* p_atom_t;
typedef struct p_mutex* p_mutex_t;
typedef struct p_mutex_attr* p_mutex_attr_t;
/*
 ***********************************************************************
 * PROGRAM EXECUTION
 ***********************************************************************
 */

/*Device structure setup*/
int p_init(int type, int flags, p_dev_t dev);

/*Query certain properties within an object (tightly controlled)*/
int p_query(void* obj, int property, int *result);

/*Loads a program (or library) from the file system into memory */
int p_load(p_dev_t dev, char *file, p_program_t p);

/*Open a team of processors*/
int p_open(p_dev_t dev, int *list, p_team_t t);

/*Get symbol from the program in memory*/
int p_getsymbol(p_program_t p, char* symbol, p_symbol_t s);

/*Run a program on N processors of a device, return event*/
int p_run(p_program_t program, p_team_t team, int argn, void **args, int flags);

/*Team Barrier*/
int p_barrier(p_team_t team);

/*Memory allocation*/
int p_malloc(p_team_t team, int n, size_t size, p_mem_t mem);

/*Get a the physical address of the memory object*/
void p_getaddr(p_mem_t mem, p_memptr_t memptr);

/*Free allocated memory */
void p_free(p_mem_t mem);

/*Finalize device run time*/
int p_finalize(p_dev_t dev);

/*
 ***********************************************************************
 * LOW LEVEL SHARED MEMORY MANAGEMENT (USER SPACE)
 ***********************************************************************
 */

/*Writes to a global memory address from a local address*/
int p_write(void *src, size_t nb, int flags, p_mem_t mem);

/*Reads from a global memory address */
int p_read(p_mem_t mem, size_t nb, int flags, void *dst);    

/*Scatters data from a local array to a list of remote memory objects*/
int p_scatter(void *src, size_t nsrc, size_t ndst, int flags, void** dstlist); 

/*Gathers data from a list of remote memory objects into a local array*/
int p_gather(void** srclist, size_t nsrc, size_t ndst, int flags, void *dst); 

/*Broadcasts an array based to a list of destination pointers*/
int p_bcast(void *src, size_t nsrc, size_t ndst, int flags, void** dstlist); 

/*Flushes the read/write path to a specific memory location (blocking)*/
void p_flush(p_mem_t mem);

/*Specialized low level shared memory memcpy interface (non-blocking)*/
int p_copy(void *src, size_t nb, int flags, void *dst);

/*
 ***********************************************************************
 * SYNCHRONIZATION PRIMITIVES
 *
 ***********************************************************************
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
void p_mutex_init(p_mutex_t mutex, p_mutex_attr_t attr);

/*Lock a mutex (try until fail)*/
void p_mutex_lock(p_mutex_t mutex);

/*Try locking a mutex once*/
int  p_mutex_trylock(p_mutex_t mutex);

/*Unlock a mutex*/
void p_mutex_unlock(p_mutex_t mutex);

/*Destroy a mutex*/
void p_mutex_destroy(p_mutex_t mutex);

/*atomic fetch and add*/
void p_atomic_add_u32(p_atom_t atom, u32 n);

/*atomic fetch and subtract*/
void p_atomic_sub_u32(p_atom_t atom, u32 n);

/*atomic fetch and logical 'and'*/
void p_atomic_and_u32(p_atom_t atom, u32 n);

/*atomic fetch and logical 'xor'*/
void p_atomic_xor_u32(p_atom_t atom, u32 n);

/*atomic fetch and logical 'or'*/
void p_atomic_or_u32(p_atom_t atom, u32 n);

/*atomic fetch and logical 'nand'*/
void p_atomic_nand_u32(p_atom_t atom, u32 n);

/*atomic swap*/
void p_atomic_swap_u32(p_atom_t atom, u32 *input);

/*atomic compare and swap*/
void p_atomic_compswap_u32(p_atom_t atom, u32 *input, u32 desired);
