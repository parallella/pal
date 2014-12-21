
/*Is this even needed or can we get away with using standard libraries*/ 

/*shared memory stuff, just put it in posix, right place*/

/*anything above that layer has been covered by things like zeromq*/

/*just make sure the posix implementation covers bare metal as well*/
/*
 * Essential parallel programming primitives  (work with 32/64 bit addresses)
 * Question: are there standard interfaces here and can we use 
 */

/*mutex (posix and gcc builtin) inspired), same arguments*/
void p_mutex_init(p_mutex_t *mutex, const p_mutex_attr_t *attr);
void p_mutex_lock(p_mutex_t *mutex);
int  p_mutex_trylock(p_mutex_t *mutex);
void p_mutex_unlock(p_mutex_t *mutex);
void p_mutex_destroy(p_mutex_t *mutex);

/*atomics seems non standard but useful?, in C11 and gnu libs??*/

void p_atomic_add(p_atom_t *atom, int n);
void p_atomic_sub(p_atom_t *atom, int n);
void p_atomic_and(p_atom_t *atom, int n);
void p_atomic_xor(p_atom_t *atom, int n);
void p_atomic_or(p_atom_t *atom, int n);
void p_atomic_nand(p_atom_t *atom, int n);
void p_atomic_compare_exchange(p_atom_t *atom, int n);
void p_atomic_compare_exchange_n(p_atom_t *atom, int n);

/*memory synchronization (make sure all reads/writes to region are done) */
void p_memsync(void* remote_loc);

/*barrier for all threads in work group (openCL inspired)*/
void p_barrier();

