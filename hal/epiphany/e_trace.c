/*
 * e_trace.c
 *
 *  Created on: Jan 6, 2014
 *      Author: M Taveniku
 */

#include "e_trace.h"
#include "e_lib.h"
#include "a_trace_shared.h"


/**
 * Internal settings for Trace function
 */

//#define IRQ_WRAP_TIMER

/**
 * Forward definition of internal functions
 */

/**
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) wand_trace_isr(int signum);


/**
 * Timer1 ISR
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) timer1_trace_isr(int signum);

 /* **
 * Internal MACRO definitions
 *
 */

// Hack until WAND interrupt gets implemented in Epiphany functions
#define E_WAND_INT (0x8)

/**
 * Internal static variables
 */
//unsigned long long *logDest = (unsigned long long *)(TRACE_MASTER_BASE + DMA1AUTO0);
unsigned logCoreid; //core-id shifted to enable fast trace

/**
 * Module implementation
 */
unsigned traceBufSize, traceBufStart, traceBufEnd;
unsigned long long *traceBufWrPtr;
#define TIMER_WRAP_BIT (1<<26)

/**
 * Initialize data structures, call this first before using trace functions
 */
int trace_init()
{
    e_memseg_t emem;
	unsigned coreIdx, totCores;

	coreIdx = e_group_config.core_row * e_group_config.group_cols + e_group_config.core_col; //0 .. 16
	totCores = e_group_config.group_rows * e_group_config.group_cols;

    // Attach to the shm segment
    if ( E_OK != e_shm_attach(&emem, HOST_TRACE_SHM_NAME) ) {
	    return E_ERR;
    }

	traceBufSize = HOST_TRACE_BUF_SIZE / totCores;
	traceBufStart = emem.ephy_base + (coreIdx * traceBufSize);
	traceBufEnd = traceBufStart + traceBufSize;
	traceBufWrPtr = (unsigned long long*)traceBufStart;

#ifdef IRQ_WRAP_TIMER
	unsigned regConfig;

	// Set timer to wrap without interrupt
	// setup timer 1 for work
	regConfig = e_reg_read(E_REG_CONFIG);
	regConfig = regConfig | TIMER_WRAP_BIT;
	e_reg_write(E_REG_CONFIG, regConfig);
#else
	//register timer ISR
	e_irq_global_mask(0);
	e_irq_attach(E_TIMER1_INT, timer1_trace_isr);
	e_irq_mask(E_TIMER1_INT, 0);
#endif

	e_ctimer_stop(E_CTIMER_1);
	e_ctimer_set(E_CTIMER_1, E_CTIMER_MAX);
	logCoreid = (e_get_coreid() & 0xFFF) << 8; // make it easy to use core-id

	return E_OK;
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
	unsigned irqState;
	e_irq_global_mask(E_FALSE);
	e_irq_attach(E_WAND_INT, wand_trace_isr);
	e_irq_mask(E_WAND_INT, E_FALSE);
	__asm__ __volatile__("wand"); // Set our WAND bit
	__asm__ __volatile__("idle"); // Idle and wait for an interrupt
	irqState = e_reg_read(E_REG_STATUS);
	irqState = irqState & (~0x8);  // This is the WAND interrupt flag
	e_reg_write(E_REG_FSTATUS, irqState);
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
	unsigned dta[2];
	dta[1] = severity | event | breakpoint | logCoreid | data;
	dta[0] = e_ctimer_get(E_CTIMER_1);

	*traceBufWrPtr++ = *(unsigned long long *)dta;
	if((unsigned)traceBufWrPtr >= traceBufEnd) traceBufWrPtr = (unsigned long long*)traceBufStart;
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
	(void) signum;
	e_ctimer_set(E_CTIMER_1, E_CTIMER_MAX);
	e_ctimer_start(E_CTIMER_1, E_CTIMER_CLK);
	return;
}

/**
 * stop this trace, free resources
 */
int trace_stop()
{
	e_ctimer_stop(E_CTIMER_1);
	e_irq_mask(E_TIMER1_INT, E_TRUE);
	return 0;
}

/**
 * This routine is installed with the interrupt Attach function
 * There will be no signal number attached when invoked
 * @param signum: not used
 */
void __attribute__((interrupt)) wand_trace_isr(int signum)
{
	(void)signum;
	//Intentionally left empty
}


