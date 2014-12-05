/*
 * Essential parallel programming primitives
 *
 */

/*mutex*/
void p_mutex_init(pal_dev_t dev, int n, p_mutex_t *mutex, 
		    const p_mutexattr_t *attr);
void p_mutex_lock(pal_dev_t dev, int n, p_mutex_t *mutex);
int  p_mutex_trylock(pal_dev_t dev, int n, p_mutex_t *mutex);
void p_mutex_unlock(pal_dev_t dev, int n, p_mutex_t *mutex);
void p_mutex_destroy(pal_dev_t dev, int n, p_mutex_t *mutex);

/*atomics*/
void p_atomic_add(pal_dev_t dev, int n, int n);
void p_atomic_sub(pal_dev_t dev, int n, int n);
void p_atomic_and(pal_dev_t dev, int n, int n);
void p_atomic_xor(pal_dev_t dev, int n, int n);
void p_atomic_or(pal_dev_t dev, int n, int n);
void p_atomic_nand(pal_dev_t dev, int n, int n);
void p_atomic_compare_exchange(pal_dev_t dev, int n, int n);
void p_atomic_compare_exchange_n(pal_dev_t dev, int n, int n);

/*memory synchronization*/
void p_mem_sync(pal_dev_t dev, int n, void *pilot);

/*barrier for all threads in work group*/
void p_barrier();

/*conditionals*/
void p_cond_init(e_cond_t *cond);
void p_cond_wait(e_cond_t *cond, e_mutex_t *mutex);
void p_cond_signal(e_cond_t *cond, e_mutex_t *mutex);
void p_cond_broadcast(e_cond_t *cond);




