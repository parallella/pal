
#include "e_coreid.h"

void e_coords_from_coreid(e_coreid_t coreid, unsigned *row, unsigned *col)
{
	coreid = coreid - e_group_config.group_id;

	*row = (coreid >> 6) & 0x3f;
	*col = coreid & 0x3f;

	return;
}
