

#include "e_coreid.h"

/* Is address on this core? */
e_bool_t e_is_on_core(const void *ptr)
{
	unsigned coreid;
	e_bool_t res;

	coreid = (((unsigned) ptr) & 0xfff00000) >> 20;

	if (coreid == e_get_coreid())
		res = E_TRUE;
	else if (coreid == 0)
		res = E_TRUE;
	else
		res = E_FALSE;

	return res;
}
