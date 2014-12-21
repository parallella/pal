
#include "e_coreid.h"


e_coreid_t e_coreid_from_coords(unsigned row, unsigned col)
{
	e_coreid_t coreid;

	coreid = ((row & 0x3f) << 6) | (col & 0x3f);
	coreid = coreid + e_group_config.group_id;

	return coreid;
}
