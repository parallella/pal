/*
 ***********************************************************************
 * PROGRAM EXECUTION
 *
 ***********************************************************************
 *
 *  Goals:
 * -Support multiple device types (ARM, FPGA, Epiphany, GPU) 
 * -Supports multiple linux processes interacting with a device independently
 * -Supports a group of cooperating threads within a group within a process
 * (only possible of there are multiple processes, coprocessor model)
 */

/*Initializes the PAL run time*/
/*This should create an opaque structure)*/
/*Assume you can have multiple devices*/
/*This has to be here, don't assume O/S support*/
/*ARM ASYNC
  ARM+FPGA (same interface)
  This should return a set of IDs
  There needs to be some information about topology as well
  0..N
  N x M
  N x M x K
  this can be done in open, request as 2D array, 1 D array
*/ 

/*connects to a device*/
/*turns it on if it was off*/
/*issues reset if we are the first ones*/
/*where to read the setup information from*/
pal_dev_t *p_init(int flags);

/*Opens connection to a set of slave processors*/
pal_team_t *p_open(pal_dev_t *dev, 
		   pal_pids_t *list,
		   int flags
		   );
/*
 *Loads a program file into memory from the file system
 *could be a bit stream, elf, etc
*/
pal_program_t *p_load(pal_dev_t *dev, char *exe);

/*Run a program on N processors of a device*/
void p_run (pal_program_t *prog, //program to run
            pal_team_t *team,    //team of processors to run on
            int np,              //processor offset within team (0..N-1),  
            int nargs,           //number of program arguments
            void** args,         //array of pointers to program arguments
            int flags            //launch flags ("ALL"), default is one
           );


/*Notifies run time, that you are done with HW
 *Used for power saving
 */

void p_finalize(pal_dev_t);

/*
 ***********************************************************************
 * LOW LEVEL MULTICORE MEMORY MANAGEMENT (USER SPACE)
 *
 ***********************************************************************
 */

/*Do we need the dev_t and mem_t at this level???*/
/*physical vs virtual*/
/*should work on epiphany and arm*/
/*adding one level of redirection should resolve this*/
/*make the global address a different type?*/

/*Copies from one global source addr to another  global destination addr*/
void *p_copy(void *dst, const void *src, size_t nb, int flags);

/*Writes to a global memory address from a local address*/
void *p_write(void *dst, const void *loc_src, size_t nb, int flags);

/*Reads from a global memory address */
void *p_read(void *loc_dst, const void *src, size_t nb, int flags);    

/*Scatters an array based on a list of destination pointers*/
void *p_scatter(void *dstlist, const void *loc_src, size_t nb, int nd, int flags);

/*Gathers an array based on a list of source pointers*/
void *p_gather(void *loc_dst, const void *srclist, size_t nb, int ns, int flags);

/*Broadcasts an array based to a list of destination pointers*/
void *p_bcast(void *dstlist, const void *loc_src, size_t nb, int nd, int flags););

/*Allocates dynamic memory*/
void *p_malloc(size_t size););

/*Frees dynamic memory*/
void p_free(void *ptr);

/*Flushes the read/write path to a specific memory location*/
void p_flush(void *ptr);

/*
 ***********************************************************************
 * SYNCHRONIZATION PRIMITIVES
 *
 ***********************************************************************
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
void p_mutex_init(p_mutex_t *mutex, const p_mutex_attr_t *attr);
void p_mutex_lock(p_mutex_t *mutex);
int  p_mutex_trylock(p_mutex_t *mutex);
void p_mutex_unlock(p_mutex_t *mutex);
void p_mutex_destroy(p_mutex_t *mutex);

/*atomics seems non standard but useful?, in C11 and gnu libs??*/

/*atomic fetch and add*/
void p_atomic_add_32u(p_atom_t *atom, int n);

/*atomic fetch and subtract*/
void p_atomic_sub_32u(p_atom_t *atom, int n);
/*atomic fetch and logical 'and'*/
void p_atomic_and_32u(p_atom_t *atom, int n);

/*atomic fetch and logical 'xor'*/
void p_atomic_xor_32u(p_atom_t *atom, int n);

/*atomic fetch and logical 'or'*/
void p_atomic_or_32u(p_atom_t *atom, int n);

/*atomic fetch and logical 'nand'*/
void p_atomic_nand_32u(p_atom_t *atom, int n);

/*This is the generic version of an atomic exchange. It stores the contents of *val into *ptr. The original value of *ptr is copied into *ret*/
void p_atomic_exchange_32u(p_atom_t *atom, int *val, int *ret);

/*atomic compare and exchange.This compares the contents of *atom with the contents of *expected and if equal, writes 'desired' into *atom. If they are not equal, the current contents of *atom is written into *expected. 
*/
void p_atomic_compare_exchange_32u(p_atom_t *atom, int *expected, int desired);
