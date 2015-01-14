
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <err.h>
#include <elf.h>

#include "e-loader.h"

#define diag(vN)   if (e_load_verbose >= vN)

e_return_stat_t ee_process_ELF(char *executable, e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col);
e_return_stat_t ee_process_SREC(char *executable, e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col);
int ee_set_core_config(e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col);

e_loader_diag_t e_load_verbose = 0;

/* diag_fd is set by e_set_loader_verbosity() */
FILE *diag_fd = NULL;

// TODO: replace with platform data
#define EMEM_SIZE (0x02000000)

int e_load(char *executable, e_epiphany_t *dev, unsigned row, unsigned col, e_bool_t start)
{
	int status;

	status = e_load_group(executable, dev, row, col, 1, 1, start);

	return status;
}


int e_load_group(char *executable, e_epiphany_t *dev, unsigned row, unsigned col, unsigned rows, unsigned cols, e_bool_t start)
{
	e_mem_t      emem, *pemem;
	unsigned int irow, icol;
	int          status;
	FILE        *fp;
	char         hdr[5] = {'\0', '\0', '\0', '\0', '\0'};
	char         elfHdr[4] = {ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3};
	char         srecHdr[2] = {'S', '0'};
	e_bool_t     iself;
	e_return_stat_t retval;

	status = E_OK;
	iself  = E_FALSE;

	pemem = &emem;

	if (dev && pemem)
	{
        // Allocate External DRAM for the epiphany executable code
        // TODO: this is barely scalable. Really need to test ext. mem size to load
		// and possibly split the ext. mem accesses into 1MB chunks.
		if (e_alloc(pemem, 0, EMEM_SIZE))
		{
			warnx("\nERROR: Can't allocate external memory buffer!\n\n");
            return E_ERR;
		}

		if (executable[0] != '\0')
		{
			if ( (fp = fopen(executable, "rb")) != NULL )
			{
				fseek(fp, 0, SEEK_SET);
				fread(hdr, 1, 4, fp);
				fclose(fp);
			} else {
				warnx("ERROR: Can't open executable file \"%s\".\n", executable);
				e_free(pemem);
				return E_ERR;
			}

			if (!strncmp(hdr, elfHdr, 4))
			{
				iself = E_TRUE;
				diag(L_D1) { fprintf(diag_fd, "e_load_group(): loading ELF file %s ...\n", executable); }
			}
			else if (!strncmp(hdr, srecHdr, 2))
			{
				diag(L_D1) { fprintf(diag_fd, "e_load_group(): loading SREC file %s ...\n", executable); }
			}
			else
			{
				diag(L_D1) { fprintf(diag_fd, "e_load_group(): Executable header %02x %02x %02x %02x\n",
                                     hdr[0], hdr[1], hdr[2], hdr[3]); }
				warnx("ERROR: Can't load executable file: unidentified format.\n");
				e_free(pemem);
				return E_ERR;
			}

			for (irow=row; irow<(row+rows); irow++)
				for (icol=col; icol<(col+cols); icol++)
				{
					if (iself)
						retval = ee_process_ELF(executable, dev, pemem, irow, icol);
					else
						retval = ee_process_SREC(executable, dev, pemem, irow, icol);

					if (retval == E_ERR)
					{
						warnx("ERROR: Can't load executable file \"%s\".\n", executable);
						e_free(pemem);
						return E_ERR;
					} else
						ee_set_core_config(dev, pemem, irow, icol);
				}

			if (start)
				for (irow=row; irow<(row+rows); irow++)
					for (icol=col; icol<(col + cols); icol++)
						{
							diag(L_D1) { fprintf(diag_fd, "e_load_group(): send SYNC signal to core (%d,%d)...\n", irow, icol); }
							e_start(dev, irow, icol);
							diag(L_D1) { fprintf(diag_fd, "e_load_group(): done.\n"); }
						}

			diag(L_D1) { fprintf(diag_fd, "e_load_group(): done loading.\n"); }
		}

		e_free(pemem);
		diag(L_D1) { fprintf(diag_fd, "e_load_group(): closed connection.\n"); }
	}
	else
	{
		warnx("ERROR: Can't connect to Epiphany or external memory.\n");
		return E_ERR;
	}

	diag(L_D1) { fprintf(diag_fd, "e_load_group(): leaving loader.\n"); }

	return status;
}


int ee_set_core_config(e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col)
{
	e_group_config_t *pgrpcfg  = (void *) SIZEOF_IVT;
	e_emem_config_t  *pememcfg = (void *) SIZEOF_IVT + sizeof(e_group_config_t);

	e_group_config_t e_group_config;
	e_emem_config_t  e_emem_config;

	e_group_config.objtype     = E_EPI_GROUP;
	e_group_config.chiptype    = pEpiphany->type;
	e_group_config.group_id    = pEpiphany->base_coreid;
	e_group_config.group_row   = pEpiphany->row;
	e_group_config.group_col   = pEpiphany->col;
	e_group_config.group_rows  = pEpiphany->rows;
	e_group_config.group_cols  = pEpiphany->cols;
	e_group_config.core_row    = row;
	e_group_config.core_col    = col;

	e_group_config.alignment_padding = 0xdeadbeef;

	e_emem_config.objtype   = E_EXT_MEM;
	e_emem_config.base      = pEMEM->ephy_base;

	e_write(pEpiphany, row, col, (off_t) pgrpcfg,  &e_group_config, sizeof(e_group_config_t));
	e_write(pEpiphany, row, col, (off_t) pememcfg, &e_emem_config,  sizeof(e_emem_config_t));

	return 0;
}


e_loader_diag_t e_set_loader_verbosity(e_loader_diag_t verbose)
{
	e_loader_diag_t old_load_verbose;

	old_load_verbose = e_load_verbose;
	diag_fd = stderr;
	e_load_verbose = verbose;
	diag(L_D1) { fprintf(diag_fd, "e_set_loader_verbosity(): setting loader verbosity to %d.\n", verbose); }
	e_set_host_verbosity(verbose);

	return old_load_verbose;
}

