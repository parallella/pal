

#include <e_coreid.h>
#include <e_mutex.h>

// TODO: use asingle pointer to store barrier addresses
void e_barrier_init(volatile e_barrier_t bar_array[], e_barrier_t *tgt_bar_array[])
{
	unsigned int corenum, numcores, i, j;

	numcores = e_group_config.group_rows * e_group_config.group_cols;
	corenum  = e_group_config.core_row * e_group_config.group_cols + e_group_config.core_col;

	for (i=0; i<numcores; i++)
		bar_array[i] = 0;

	if (corenum == 0)
	{
		for (i=0; i<e_group_config.group_rows; i++)
			for (j=0; j<e_group_config.group_cols; j++)
				tgt_bar_array[corenum++] = (e_barrier_t *) e_get_global_address(i, j, (void *) &(bar_array[0]));
	} else {
		tgt_bar_array[0] = (e_barrier_t *) e_get_global_address(0, 0, (void *) &(bar_array[corenum]));
	}

	return;
}
