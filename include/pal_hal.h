/*
 * Hardware Abstraction Library
 */

/*support multiple processes running in parallel*/
/*support bare metal non O/S and O/S functions*/

/*open a device/connection, one "core" at a time, open core for ARM as well, use the p_dev_t as the */
/*this sends down the physical address to that core, containing the pointer to the p_dev_t*/
/*wrap some foor loops around this*/
/*memcpy works with physical addresses*/
/*keep interface simple, keep topology outside the function*/
/*physical vs virtual addresses, pointer should be the one provided underlying O/S*/
/*in bare metal VM==PM*/
/*always work on malloced memory*/

/*initialize device at beginning of program, get information from device tree, imagine multiple hetero devices*/
/*memory map should be described by driver/bsp file here*/
/*everything passed as arguments rather than environment variables, transparent/explicit*/
/*all types are exposed "enough", meaning what information needs to be in it, creates a structure that can be accessed*/
//1.)gets system info from linux driver or from linux.dts file passed in here in bare metal system
//2.)creates a local data structure with the system world described
//3.) We can have multiple asynchronous work groups inside this one process 
p_dev_t p_init( char* src, int flags );

/*open/reserve a device, needs interface work!, agnostic, transparent, this is, passes p_dev_t to each device in universe, makes a circuit connection*/
//1.
//2.on that and each subsequent call copies the device data (current host) into structure on the slave device
//3.sets up some parameters to be used by subsequent calls used by broadcast , load code etc
p_workgroup_t p_open( p_dev_t dev, int cpu, int flags );

/*malloc, allocate memory locally*/
/*what about remote, no that would be completely different code, should not be in the same function*/
/*need to request memory, this is a higher task*/
p_mem_t *p_malloc( p_dev_t dev, size_t size, int flags );//arguments??

/*free memory*/
void p_free( p_dev_t dev, p_mem_t mem );

/*get allocated memory poiter*/
void* p_memptr( cp_mem_t mem, int flag );

/*load a program into memory*/
int p_readelf( p_dev_t dev, char* src, size_t len, char* opt, char** log);

/*get a symbol from elf*/
p_sym_t p_getsymbol( p_program_t prg, const char* symbol );

/*get a memory pointer*/
p_memptr( (coprthr_mem_t)mtx,0);

/*load and execute a program on one core*/
int p_load( p_dev_t dev, int cpu, p_kernel_t krn, unsigned int narg, void** args);

/*load and execute program on all opened CPUs "broadcast to workgroup"*/
int p_load_bcast( p_dev_t dev, p_kernel_t krn, unsigned int narg, void** args );

/*memcpy from cpu0 to cpu1, most of them should use some kind of hw opt like DMA*/
size_t *p_memcpy( p_dev_t dev, p_mem_t mem_src, size_t offset_src, p_mem_t mem_dst, size_t offset_dst, size_t len, int flags );

/*writing to remote core*/
size_t *p_write( p_dev_t dev, p_mem_t mem_src, size_t offset_src, p_mem_t mem_dst, size_t offset_dst, size_t len, int flags );

/*reading from remote core*/
size_t *p_read( p_dev_t dev, p_mem_t mem_src, size_t offset_src, p_mem_t mem_dst, size_t offset_dst, size_t len, int flags );

/*broadcast data to all of workgroup on channel "X"*/
size_t *p_bcast( p_dev_t dev, p_mem_t mem_src, size_t offset_src, p_mem_t mem_dst, size_t offset_dst, size_t len, int flags );

/*close a device/connection, one "core at a time"*/
int p_close( p_workgroup_t *wg );

/*shut down the device at end of program*/
p_dev_t p_finalize();

/* interact with one of n counters, counts from 0 up *(ignore decrementing nature)*/
void p_counter_set(int n, unsigned int val);
void p_counter_get(int n);
void p_counter_start(int n, p_timer_config_t);
void p_counter_stop(int n);

/*signal function..need to rethink this one...*/
//void p_signal(e_epiphany_t *dev, int cpu);







		      						      


//Example program
