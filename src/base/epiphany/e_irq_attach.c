
#include <e_ic.h>

#define B_OPCODE 0x000000e8 // OpCode of the B<*> instruction

#undef _USE_SIGNAL_
#ifndef _USE_SIGNAL_

void e_irq_attach(e_irq_type_t irq, sighandler_t handler)
{
	unsigned *ivt;

	// The event vector is a relative branch instruction to the IRS.
	// To use the direct ISR dispatch, we need to re-program the
	// IVT entry with the new branch instruction.

	// Set TIMER0 IVT entry address
	ivt  = (unsigned *) (irq << 2);
	// Set the relative branch offset.
	*ivt = (((unsigned) handler - (unsigned) ivt) >> 1) << 8;
	// Add the instruction opcode.
	*ivt = *ivt | B_OPCODE;

	return;
}

#else
#include <machine/epiphany_config.h>
#include <signal.h>

void e_irq_attach(e_irq_type_t irq, sighandler_t handler)
{
	signal(((irq - E_SYNC) + SIG_RESET), handler);

	return;
}

#endif
