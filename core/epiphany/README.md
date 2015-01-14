"TODO" list for the old ESDK
==================================

comments: be constistent, underscor instead of "-"

* e_trace_dma.c - REPLACE, in favor of http://lttng.org/blog/2014/11/25/tracing-bare-metal-systems/
* e_trace.c     - REPLACE, in favor of lttng
* e_shm.c       - review..???
* e_reg_write.c - keep, good abstraction layer for register write, how generic to make?
                  make it a 32 or 64 bit bit address constant?
                  make it work for zynq as well
                  should work on any core, ARM, epiphany etc, same interface, different data
                  and addresses
* e_reg_read.c  - same as above
* epiphany-hal-data.h - Address should be ID+local, up to architecture
                        no error checking, responsibility at upper level
* epiphany-hal.c      - init, finalize, open, close, alloc, free (standardize)
* e_mutex_*,c      - make it a posix interface -->exactly posix, no silly coordinates, 
                     just an address
* e_barrier        - should there be an array of barriers or just one?
* e_memcpy         - generic interface, do detection inside
                     how many cycles could this actually cost?
* e_irq_set        - needed, comp to signal..
* e_irq_mask       - useful for most interrupt controllers                      
* e_irq_mask_all   - too specific to epiphany                    
* e_irq_clear      - useful
* e_irq_attach     - register an ISR, too slow to be useful?
* e_dma_wait       - remove, should be hidden, need better spec, examples for api writes
* e_dma_start      - same as above
* e_dma_set_desc   - seems pretty useless, still too hard
* e_dma_copy       - descriptor placement needs work, clashing, why not hide in memcpy
                     should be in memcpy, should require three values: 
                     (src,dst,count)
		     impose alignment restrictions?
		     (pre,body,post-->make safe)
* e_wait           - cancel, who needs it
* e_ctimer_stop    - make generic,
* e_ctimer_start   - make generic,
* *.s              - remove
*coreid*	   - remove

 
  



                        
                             




