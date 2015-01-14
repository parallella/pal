
#include "e_coreid.h"

void e_neighbor_id(e_coreid_wrap_t dir, e_coreid_wrap_t wrap, unsigned *nrow, unsigned *ncol)
{
	unsigned row_mask, col_mask;
	unsigned row, col;

	/* Indexed by [wrap][dir] */
	static const unsigned row_adjust[3][2] =
	{
		{ 0,  0 }, /* GROUP_WRAP */
		{ 0,  0 }, /* ROW_WRAP  */
		{-1,  1 }  /* COL_WRAP  */
	};

	static const unsigned col_adjust[3][2] =
	{
		{-1,  1 }, /* GROUP_WRAP */
		{-1,  1 }, /* ROW_WRAP  */
		{ 0,  0 }  /* COL_WRAP  */
	};

	/* This only works for Power-Of-Two group dimensions */
	row_mask = e_group_config.group_rows - 1;
	col_mask = e_group_config.group_cols - 1;


	/* Calculate the next core coordinates */
	row = e_group_config.core_row + row_adjust[wrap][dir];
	col = e_group_config.core_col + col_adjust[wrap][dir];

	if (wrap == E_GROUP_WRAP)
		/* when the new col is negative, it is wrapped around due to the unsignedness
		 * of the variable. I any case, an edge core's column gets greater than group
		 * size
		 */
		if (col >= e_group_config.group_cols)
			row = row + (2 * dir - 1);

	*nrow = row & row_mask;
	*ncol = col & col_mask;

	return;
}
