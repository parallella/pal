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
 * RUN-TIME DEFINITIONS
 ***********************************************************************
 */
#define SMP       0x01
#define OPENCL    0x02
#define CLUSTER   0x04
#define EPIPHANY  0x08
#define FPGA      0x10

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

struct pal_dev;
struct pal_team;
struct pal_program;
struct pal_symbol;
struct pal_event;
struct pal_mem;
struct pal_memptr;
struct pal_atom;
struct pal_mutex;
struct pal_mutex_attr;

typedef struct pal_dev* pal_dev_t;
typedef struct pal_team* pal_team_t;
typedef struct pal_program* pal_program_t;
typedef struct pal_symbol* pal_symbol_t;
typedef struct pal_event* pal_event_t;
typedef struct pal_mem* pal_mem_t;
typedef struct pal_memptr* pal_memptr_t;
typedef struct pal_atom* pal_atom_t;
typedef struct pal_mutex* pal_mutex_t;
typedef struct pal_mutex_attr* pal_mutex_attr_t;
/*
 ***********************************************************************
 * PROGRAM EXECUTION
 ***********************************************************************
 */

/*Device structure setup*/
int pal_init(int type, int flags, pal_dev_t dev);

/*Query the device for certain properties*/
int pal_query(pal_dev_t dev, int property, int *result);

/*Loads a program (or library) from the file system into memory */
int pal_load(pal_dev_t dev, char *file, pal_prog_t prog);

/*Open a team of processors*/
int pal_open(pal_dev_t dev, int *list, int flags, pal_team_t team);

/*Get symbol from the program in memory*/
int pal_getsymbol(pal_program_t prog, char* symbol, pal_symbol_t symbol);

/*Run a program on N processors of a device, return event*/
int pal_run(pal_team_t team, pal_program_t prog, int argn, void **args, int flags, pal_event_t event);

/*Team Barrier*/
int pal_barrier(pal_team_t team);

/*Memory allocation*/
int pal_malloc(pal_team_t team, int n, size_t size, pal_mem_t mem);

/*Get a the physical address of the memory object*/
void pal_getaddr(pal_mem_t mem, pal_memptr_t memptr);

/*Free allocated memory */
void pal_free(pal_mem_t mem);

/*Finalize device run time*/
int pal_finalize(pal_dev_t dev);

/*
 ***********************************************************************
 * LOW LEVEL SHARED MEMORY MANAGEMENT (USER SPACE)
 ***********************************************************************
 */

/*Writes to a global memory address from a local address*/
int pal_write(void *src, size_t nb, int flags, pal_mem_t mem, pal_event_t event);

/*Reads from a global memory address */
int pal_read(pal_mem_t mem, size_t nb, int flags, void *dst, pal_event_t event);    

/*Scatters data from a local array to a list of remote memory objects*/
int pal_scatter(void *src, size_t nsrc, size_t ndst, int flags, void** dstlist, pal_event_t event); 

/*Gathers data from a list of remote memory objects into a local array*/
int pal_gather(void** srclist, size_t nsrc, size_t ndst, int flags, void *dst, pal_event_t event); 

/*Broadcasts an array based to a list of destination pointers*/
int pal_bcast(void *src, size_t nsrc, size_t ndst, int flags, void** dstlist, pal_event_t event); 

/*Flushes the read/write path to a specific memory location (blocking)*/
void pal_flush(pal_mem_t mem);

/*Specialized low level shared memory memcpy interface (non-blocking)*/
int pal_copy(void *src, size_t nb, int flags, void *dst);

/*
 ***********************************************************************
 * SYNCHRONIZATION PRIMITIVES
 *
 ***********************************************************************
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
void pal_mutex_init(pal_mutex_t mutex, pal_mutex_attr_t attr);

/*Lock a mutex (try until fail)*/
void pal_mutex_lock(pal_mutex_t mutex);

/*Try locking a mutex once*/
int  pal_mutex_trylock(pal_mutex_t mutex);

/*Unlock a mutex*/
void pal_mutex_unlock(pal_mutex_t mutex);

/*Destroy a mutex*/
void pal_mutex_destroy(pal_mutex_t mutex);

/*atomic fetch and add*/
void pal_atomic_add_32u(pal_atom_t atom, unsigned int n);

/*atomic fetch and subtract*/
void pal_atomic_sub_32u(pal_atom_t atom, unsigned int n);

/*atomic fetch and logical 'and'*/
void pal_atomic_and_32u(pal_atom_t atom, unsigned int n);

/*atomic fetch and logical 'xor'*/
void pal_atomic_xor_32u(pal_atom_t atom, unsigned int n);

/*atomic fetch and logical 'or'*/
void pal_atomic_or_32u(pal_atom_t atom, unsigned int n);

/*atomic fetch and logical 'nand'*/
void pal_atomic_nand_32u(pal_atom_t atom, unsigned int n);

/*atomic exchange*/
void pal_atomic_exchange_32u(pal_atom_t atom, int *val, int *ret);

/*atomic compare and exchange*/
void pal_atomic_compare_exchange_32u(pal_atom_t atom, int *expected, int desired);
