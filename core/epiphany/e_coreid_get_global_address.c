
#include "e_coreid.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
void *e_get_global_address(unsigned row, unsigned col, const void *ptr)
{
	unsigned   uptr;
	e_coreid_t coreid;

	/* If the address is global, return the pointer unchanged */
	if (((unsigned) ptr) & 0xfff00000)
	{
		uptr = (unsigned) ptr;
		return (void *) uptr;
	}
	else if ((row == E_SELF) || (col == E_SELF))
		coreid = e_get_coreid();
	else
		coreid = (row * 0x40 + col) + e_group_config.group_id;

	/* Get the 20 ls bits of the pointer and add coreid. */
//	uptr = ((unsigned) ptr) & 0x000fffff; // not needed because of the 1st condition above
	uptr = (unsigned) ptr;
	uptr = (coreid << 20) | uptr;

	return (void *) uptr;
}
#pragma GCC diagnostic pop
