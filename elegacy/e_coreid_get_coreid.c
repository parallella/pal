
#include "e_coreid.h"


e_coreid_t e_get_coreid(void)
{
	register unsigned coreid_in_reg asm("r0");
	__asm__ __volatile__ ("MOVFS %0, COREID" : : "r" (coreid_in_reg));

	return (coreid_in_reg);
}
