/*
 * e_trace.c
 *
 *	Created on: Jan 6, 2014
 *		Author: M Taveniku
 */

#include "e_trace.h"
#include "e_lib.h"
#include "a_trace_shared.h"

/**
 * Forward definition of internal functions
 */


/**
 * Setup the DMA 1 Engine for trace work
 * No parameters needed
 */
int setupDMA();
int setupDMA2();

/**
 * Timer1 ISR
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) timer1_trace_isr(int signum);

/**
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) wand_trace_isr(int signum);


/**
 * Internal MACRO definitions
 *
 */

// Hack until WAND interrupt get implemented in epiphany functions
#define E_WAND_INT (0x8)
// should be 0xF0534
#define DMA1AUTO0 (0xF0534)
#define DMA1AUTO1 (0xF0538)
#define TRACE_MASTER_LOW_REG *(unsigned *)(TRACE_MASTER_BASE + DMA1AUTO0)
#define TRACE_MASTER_HIGH_REG *(unsigned *)(TRACE_MASTER_BASE + DMA1AUTO1)

//#define TRACE_LOG_DEST (unsigned long long *)(TRACE_MASTER_BASE | DMA1AUTO0)

/**
 * Internal static variables
 */
//unsigned long long *logDest = (unsigned long long *)(TRACE_MASTER_BASE + DMA1AUTO0);
unsigned logCoreid; //core-id shifted to enable fast trace
e_dma_desc_t dmaDesc; //used for trace - need to be static
e_dma_desc_t dmaDesc2; //used for trace - need to be static

/**
 * Module implementation
 */

/**
 * Initialize data structures, call this first before using trace functions
 */
int trace_init()
{
	// If I am the master
	if(e_get_coreid() == TRACE_MASTER_ID) {
		// setup my DMA engine
		setupDMA2(); // REMEMBER TO CHANGE
	}

	//register timer ISR
	e_irq_global_mask(0);
	e_irq_attach(E_TIMER1_INT, timer1_trace_isr);
	e_irq_mask(E_TIMER1_INT, 0);

	// setup timer 1 for work
	e_ctimer_stop(E_CTIMER_1);
	e_ctimer_set(E_CTIMER_1, E_CTIMER_MAX);
	logCoreid = (e_get_coreid()& 0xFFF) << 8; // make it easy to use core-id
	return 0;
}

/**
 * This function starts the clock counter, tracing can be used after this call
 */
int trace_start()
{
	e_ctimer_start(E_CTIMER_1, E_CTIMER_CLK);
	return 0;
}
/**
 * Start trace, but wait until all processors on this chip are ready and waiting to start
 */
int trace_start_wait_all()
{
	e_irq_attach(E_WAND_INT, wand_trace_isr);
	e_irq_mask(E_WAND_INT, 0);
	__asm__ __volatile__("wand"); // Set our WAND bit
	__asm__ __volatile__("idle"); // Idle and wait for an interrupt
	e_ctimer_start(E_CTIMER_1, E_CTIMER_CLK); // Start counting
	return 0;
}

/**
 * Write the event "event" to  log with data
 * @param severity - 0 .. 3
 * @param event - event id
 * @param breakpoint - place in user code that we hit
 * @param data - data associated with the event
 */
int trace_write(unsigned severity, unsigned event, unsigned breakpoint, unsigned data)
{
	unsigned dta;
	(void)severity;
	(void)event;
	(void)breakpoint;

	//dta = severity | event | breakpoint | logCoreid | data;
	dta = data ;
	// write the data to our auto DMA register
	TRACE_MASTER_LOW_REG = e_ctimer_get(E_CTIMER_1);
	TRACE_MASTER_HIGH_REG = dta;
	return 0;
}

/**
 * stop this trace, free resources
 */
int trace_stop()
{
	// make sure we stop things
	e_ctimer_stop(E_CTIMER_1);
	e_irq_mask(E_TIMER1_INT, 1);
	// unregister ISR?
	return 0;
}

/**
 * Setup the DMA 1 Engine for trace work
 * No parameters needed
 */
int setupDMA()
{
	/* Setup a slave DMA to point to the host buffer */
	/* right now it is setup to transfer 1M words in a circular buffer */
	e_dma_set_desc(E_DMA_1 /* channel */, (E_DMA_ENABLE | E_DMA_DWORD | E_DMA_CHAIN)
			, &dmaDesc /* *next (infinite loop) */
			, 0 /* i_stride src */, 1 /* i_stride dst */
			, 1024 /* inner cnt */	, 256 /* outer cnt */
			, 0 /* o_stride src */ , 1024 /* o_stride dst */
			, 0 /* src addr - not used */
			, (void*)(e_emem_config.base + HOST_TRACE_BUF_OFFSET) /*dst*/
			, &dmaDesc
			);
	e_dma_start(&dmaDesc, E_DMA_1); // start DMA engine go forever
	return 0;
}

/**
 * Setup the DMA 1 Engine for trace work
 * No parameters needed
 */
int setupDMA2()
{
	/* Setup a slave DMA to point to the host buffer */
	/* right now it is setup to transfer 1M words in a circular buffer */
	e_dma_set_desc(E_DMA_1 /* channel */, (E_DMA_ENABLE | E_DMA_DWORD | E_DMA_CHAIN)
			, &dmaDesc /* *next (infinite loop) */
			, 0 /* i_stride src */, 8 /* i_stride dst */
			, 8 /* inner cnt */, 1 /* outer cnt */
			, 0 /* o_stride src */, 0 /* o_stride dst */
			, 0 /* src addr - not used */
			, (void*)(e_emem_config.base + HOST_TRACE_BUF_OFFSET) /*dst*/
			, &dmaDesc
			);
	e_dma_start(&dmaDesc, E_DMA_1); // start DMA engine go forever
	return 0;
}


/**
 * Timer1 ISR
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) timer1_trace_isr(int signum)
{
   (void)signum;

	e_ctimer_set(E_CTIMER_1, E_CTIMER_MAX);
	e_ctimer_start(E_CTIMER_1, E_CTIMER_CLK);
	return;
}

/**
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) wand_trace_isr(int signum)
{
	(void)signum;
	return;
}


