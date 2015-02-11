
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <err.h>
#include <stdio.h>
#include <stdlib.h>

#include "e-hal.h"
#include "epiphany-shm-manager.h"	/* For private APIs */
#include "epiphany-hal-api-local.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

typedef unsigned int  uint;
typedef unsigned long ulong;


#define diag(vN) if (e_host_verbose >= vN)

//static int e_host_verbose = 0;
int	  e_host_verbose; //__attribute__ ((visibility ("hidden"))) = 0;
FILE *diag_fd			  __attribute__ ((visibility ("hidden")));

char *OBJTYPE[64] = {"NULL", "EPI_PLATFORM", "EPI_CHIP", "EPI_GROUP", "EPI_CORE", "EXT_MEM"};

char const esdk_path[] = "EPIPHANY_HOME";
char const hdf_env_var_name[] = "EPIPHANY_HDF";

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
e_platform_t e_platform = { E_EPI_PLATFORM };
#pragma GCC diagnostic pop

/////////////////////////////////
// Device communication functions
//
// Platform configuration
//
// Initialize Epiphany platform according to configuration found in the HDF
int e_init(char *hdf)
{
	char *hdf_env, *esdk_env, hdf_dfl[1024];
	int i;

	// Init global file descriptor
	diag_fd = stderr;

	e_platform.objtype     = E_EPI_PLATFORM;
	e_platform.hal_ver     = 0x050d0705; // TODO: update ver
	e_platform.initialized = E_FALSE;
	e_platform.num_chips   = 0;
	e_platform.num_emems   = 0;

	// Parse HDF, get platform configuration
	if (hdf == NULL)
	{
		// Try getting HDF from EPIPHANY_HDF environment variable
		hdf_env = getenv(hdf_env_var_name);
		diag(H_D2) { fprintf(diag_fd, "e_init(): HDF ENV = %s\n", hdf_env); }
		if (hdf_env != NULL)
			hdf = hdf_env;
		else
		{
			// Try opening .../bsps/current/platform.hdf
			warnx("e_init(): No Hardware Definition File (HDF) is specified. Trying \"platform.hdf\".");
			esdk_env = getenv(esdk_path);
			strncpy(hdf_dfl, esdk_env, sizeof(hdf_dfl));
			strncat(hdf_dfl, "/bsps/current/platform.hdf", sizeof(hdf_dfl));
			hdf = hdf_dfl;
		}
	}

	diag(H_D2) { fprintf(diag_fd, "e_init(): opening HDF %s\n", hdf); }
	if (ee_parse_hdf(&e_platform, hdf))
	{
		warnx("e_init(): Error parsing Hardware Definition File (HDF).");
		return E_ERR;
	}

	// Populate the missing platform parameters according to platform version.
	for (i=0; i<e_platform.num_chips; i++)
	{
		ee_set_platform_params(&e_platform);
	}

	// Populate the missing chip parameters according to chip version.
	for (i=0; i<e_platform.num_chips; i++)
	{
		ee_set_chip_params(&(e_platform.chip[i]));
	}

	// Find the minimal bounding box of Epiphany chips. This defines the reference frame for core-groups.
	e_platform.row	= 0x3f;
	e_platform.col	= 0x3f;
	e_platform.rows = 0;
	e_platform.cols = 0;
	for (i=0; i<e_platform.num_chips; i++)
	{
		if (e_platform.row > e_platform.chip[i].row)
			e_platform.row = e_platform.chip[i].row;

		if (e_platform.col > e_platform.chip[i].col)
			e_platform.col = e_platform.chip[i].col;

		if (e_platform.rows < (e_platform.chip[i].row + e_platform.chip[i].rows - 1))
			e_platform.rows =  e_platform.chip[i].row + e_platform.chip[i].rows - 1;

		if (e_platform.cols < (e_platform.chip[i].col + e_platform.chip[i].cols - 1))
			e_platform.cols =  e_platform.chip[i].col + e_platform.chip[i].cols - 1;

	}
	e_platform.rows = e_platform.rows - e_platform.row + 1;
	e_platform.cols = e_platform.cols - e_platform.col + 1;
	diag(H_D2) { fprintf(diag_fd, "e_init(): platform.(row,col)	  = (%d,%d)\n", e_platform.row, e_platform.col); }
	diag(H_D2) { fprintf(diag_fd, "e_init(): platform.(rows,cols) = (%d,%d)\n", e_platform.rows, e_platform.cols); }

	if ( E_OK != e_shm_init() ) {
		warnx("e_init(): Failed to initialize the Epiphany Shared Memory Manager.");
		return E_ERR;
	}

	e_platform.initialized = E_TRUE;

	return E_OK;
}


// Finalize connection with the Epiphany platform; Free allocated resources.
int e_finalize(void)
{
	if (e_platform.initialized == E_FALSE)
	{
		warnx("e_finalize(): Platform was not initiated.");
		return E_ERR;
	}

	e_shm_finalize();

	e_platform.initialized = E_FALSE;

	free(e_platform.chip);
	free(e_platform.emem);

	return E_OK;
}


int e_get_platform_info(e_platform_t *platform)
{
	if (e_platform.initialized == E_FALSE)
	{
		warnx("e_get_platform_info(): Platform was not initialized. Use e_init().");
		return E_ERR;
	}

	*platform = e_platform;
	platform->chip = NULL;
	platform->emem = NULL;

	return E_OK;
}


// Epiphany access
//
// Define an e-core workgroup
int e_open(e_epiphany_t *dev, unsigned row, unsigned col, unsigned rows, unsigned cols)
{
	int irow, icol;
	e_core_t *curr_core;

	if (e_platform.initialized == E_FALSE)
	{
		warnx("e_open(): Platform was not initialized. Use e_init().");
		return E_ERR;
	}

	dev->objtype = E_EPI_GROUP;
	dev->type	 = e_platform.chip[0].type; // TODO: assumes one chip type in platform

	// Set device geometry
	// TODO: check if coordinates and size are legal.
	diag(H_D2) { fprintf(diag_fd, "e_open(): platform.(row,col)=(%d,%d)\n", e_platform.row, e_platform.col); }
	dev->row		 = row + e_platform.row;
	dev->col		 = col + e_platform.col;
	dev->rows		 = rows;
	dev->cols		 = cols;
	dev->num_cores	 = dev->rows * dev->cols;
	diag(H_D2) { fprintf(diag_fd, "e_open(): dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d), num_cores=%d\n", dev->row, dev->col, dev->rows, dev->cols, row, col, dev->num_cores); }
	dev->base_coreid = ee_get_id_from_coords(dev, 0, 0);

	diag(H_D2) { fprintf(diag_fd, "e_open(): group.(row,col),id = (%d,%d), 0x%03x\n", dev->row, dev->col, dev->base_coreid); }
	diag(H_D2) { fprintf(diag_fd, "e_open(): group.(rows,cols),numcores = (%d,%d), %d\n", dev->rows, dev->cols, dev->num_cores); }


	// Open memory device
	dev->memfd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
	if (dev->memfd == -1)
	{
		warnx("e_open(): EPIPHANY_DEV file open failure.");
		return E_ERR;
	}


	// Map individual cores to virtual memory space
	dev->core = (e_core_t **) malloc(dev->rows * sizeof(e_core_t *));
	if (!dev->core)
	{
		warnx("e_open(): Error while allocating eCore descriptors.");
		return E_ERR;
	}

	for (irow=0; irow<dev->rows; irow++)
	{
		dev->core[irow] = (e_core_t *) malloc(dev->cols * sizeof(e_core_t));
		if (!dev->core[irow])
		{
			warnx("e_open(): Error while allocating eCore descriptors.");
			return E_ERR;
		}

		for (icol=0; icol<dev->cols; icol++)
		{
			diag(H_D2) { fprintf(diag_fd, "e_open(): opening core (%d,%d)\n", irow, icol); }

			curr_core = &(dev->core[irow][icol]);
			curr_core->row = irow;
			curr_core->col = icol;
			curr_core->id  = ee_get_id_from_coords(dev, curr_core->row, curr_core->col);

			diag(H_D2) { fprintf(diag_fd, "e_open(): core (%d,%d), CoreID = 0x%03x\n", curr_core->row, curr_core->col, curr_core->id); }

			//	  |-------------|	  req'd map
			// +--0-+--1-+--2-+--3-+  O/S pages
			// |--x-------------|  gen'd map; x = offset

			// SRAM array
			curr_core->mems.phy_base = (curr_core->id << 20 | e_platform.chip[0].sram_base); // TODO: assumes first chip + a single chip type
			curr_core->mems.page_base = ee_rndl_page(curr_core->mems.phy_base);
			curr_core->mems.page_offset = curr_core->mems.phy_base - curr_core->mems.page_base;
			curr_core->mems.map_size = e_platform.chip[0].sram_size + curr_core->mems.page_offset;

			curr_core->mems.mapped_base = mmap(NULL, curr_core->mems.map_size, PROT_READ|PROT_WRITE, MAP_SHARED, dev->memfd, curr_core->mems.page_base);
			curr_core->mems.base = curr_core->mems.mapped_base + curr_core->mems.page_offset;

			diag(H_D2) { fprintf(diag_fd, "e_open(): mems.phy_base = 0x%08x, mems.base = 0x%08x, mems.size = 0x%08x\n", (uint) curr_core->mems.phy_base, (uint) curr_core->mems.base, (uint) curr_core->mems.map_size); }

			// e-core regs
			curr_core->regs.phy_base = (curr_core->id << 20 | e_platform.chip[0].regs_base); // TODO: assumes first chip + a single chip type
			curr_core->regs.page_base = ee_rndl_page(curr_core->regs.phy_base);
			curr_core->regs.page_offset = curr_core->regs.phy_base - curr_core->regs.page_base;
			curr_core->regs.map_size = e_platform.chip[0].regs_size + curr_core->regs.page_offset;

			curr_core->regs.mapped_base = mmap(NULL, curr_core->regs.map_size, PROT_READ|PROT_WRITE, MAP_SHARED, dev->memfd, curr_core->regs.page_base);
			curr_core->regs.base = curr_core->regs.mapped_base + curr_core->regs.page_offset;

			diag(H_D2) { fprintf(diag_fd, "e_open(): regs.phy_base = 0x%08x, regs.base = 0x%08x, regs.size = 0x%08x\n", (uint) curr_core->regs.phy_base, (uint) curr_core->regs.base, (uint) curr_core->regs.map_size); }

			if (curr_core->mems.mapped_base == MAP_FAILED)
			{
				warnx("e_open(): ECORE[%d,%d] MEM mmap failure.", curr_core->row, curr_core->col);
				return E_ERR;
			}

			if (curr_core->regs.mapped_base == MAP_FAILED)
			{
				warnx("e_open(): ECORE[%d,%d] REG mmap failure.", curr_core->row, curr_core->col);
				return E_ERR;
			}
		}
	}

	return E_OK;
}


// Close an e-core workgroup
int e_close(e_epiphany_t *dev)
{
	int irow, icol;
	e_core_t *curr_core;

	if (!dev)
	{
		warnx("e_close(): Core group was not opened.");
		return E_ERR;
	}

	for (irow=0; irow<dev->rows; irow++)
	{
		for (icol=0; icol<dev->cols; icol++)
		{
			curr_core = &(dev->core[irow][icol]);

			munmap(curr_core->mems.mapped_base, curr_core->mems.map_size);
			munmap(curr_core->regs.mapped_base, curr_core->regs.map_size);
		}

		free(dev->core[irow]);
	}

	free(dev->core);

	close(dev->memfd);

	return E_OK;
}


// Read a memory block from a core in a group
ssize_t e_read(void *dev, unsigned row, unsigned col, off_t from_addr, void *buf, size_t size)
{
	ssize_t		  rcount;
	e_epiphany_t *edev;
	e_mem_t		 *mdev;

	switch (*((e_objtype_t *) dev))
	{
	case E_EPI_GROUP:
		diag(H_D2) { fprintf(diag_fd, "e_read(): detected EPI_GROUP object.\n"); }
		edev = (e_epiphany_t *) dev;
		if (from_addr < edev->core[row][col].mems.map_size)
			rcount = ee_read_buf(edev, row, col, from_addr, buf, size);
		else {
			*((unsigned *) (buf)) = ee_read_reg(dev, row, col, from_addr);
			rcount = 4;
		}
		break;

	case E_SHARED_MEM:	// Fall-through
	case E_EXT_MEM:
		diag(H_D2) { fprintf(diag_fd, "e_read(): detected EXT_MEM object.\n"); }
		mdev = (e_mem_t *) dev;
		rcount = ee_mread_buf(mdev, from_addr, buf, size);
		break;

	default:
		diag(H_D2) { fprintf(diag_fd, "e_read(): invalid object type.\n"); }
		rcount = 0;
		return E_ERR;
	}

	return rcount;
}


// Write a memory block to a core in a group
ssize_t e_write(void *dev, unsigned row, unsigned col, off_t to_addr, const void *buf, size_t size)
{
	ssize_t		  wcount;
	unsigned int  reg;
	e_epiphany_t *edev;
	e_mem_t		 *mdev;

	switch (*((e_objtype_t *) dev))
	{
	case E_EPI_GROUP:
		diag(H_D2) { fprintf(diag_fd, "e_write(): detected EPI_GROUP object.\n"); }
		edev = (e_epiphany_t *) dev;
		if (to_addr < edev->core[row][col].mems.map_size)
			wcount = ee_write_buf(edev, row, col, to_addr, buf, size);
		else {
			reg = *((unsigned *) (buf));
			ee_write_reg(edev, row, col, to_addr, reg);
			wcount = 4;
		}
		break;

	case E_SHARED_MEM:	// Fall-through
	case E_EXT_MEM:
		diag(H_D2) { fprintf(diag_fd, "e_write(): detected EXT_MEM object.\n"); }
		mdev = (e_mem_t *) dev;
		wcount = ee_mwrite_buf(mdev, to_addr, buf, size);
		break;

	default:
		diag(H_D2) { fprintf(diag_fd, "e_write(): invalid object type.\n"); }
		wcount = 0;
		return E_ERR;
	}

	return wcount;
}


// Read a word from SRAM of a core in a group
int ee_read_word(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr)
{
	volatile int *pfrom;
	int			  data;
	ssize_t		  size;

	size = sizeof(int);
	if (((from_addr + size) > dev->core[row][col].mems.map_size) || (from_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_read_word(): writing to from_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) from_addr, (uint) size, (uint) dev->core[row][col].mems.map_size); }
		warnx("ee_read_word(): Buffer range is out of bounds.");
		return E_ERR;
	}

	pfrom = (int *) (dev->core[row][col].mems.base + from_addr);
	diag(H_D2) { fprintf(diag_fd, "ee_read_word(): reading from from_addr=0x%08x, pfrom=0x%08x\n", (uint) from_addr, (uint) pfrom); }
	data  = *pfrom;

	return data;
}


// Write a word to SRAM of a core in a group
ssize_t ee_write_word(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, int data)
{
	int		*pto;
	ssize_t	 size;

	size = sizeof(int);
	if (((to_addr + size) > dev->core[row][col].mems.map_size) || (to_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_write_word(): writing to to_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) to_addr, (uint) size, (uint) dev->core[row][col].mems.map_size); }
		warnx("ee_write_word(): Buffer range is out of bounds.");
		return E_ERR;
	}

	pto = (int *) (dev->core[row][col].mems.base + to_addr);
	diag(H_D2) { fprintf(diag_fd, "ee_write_word(): writing to to_addr=0x%08x, pto=0x%08x\n", (uint) to_addr, (uint) pto); }
	*pto = data;

	return sizeof(int);
}


// Read a memory block from SRAM of a core in a group
ssize_t ee_read_buf(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr, void *buf, size_t size)
{
	const void	 *pfrom;
	unsigned int  addr_from, addr_to, align;
	int			  i;

	if (((from_addr + size) > dev->core[row][col].mems.map_size) || (from_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_read_buf(): reading from from_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) from_addr, (uint) size, (uint) dev->core[row][col].mems.map_size); }
		warnx("ee_read_buf(): Buffer range is out of bounds.");
		return E_ERR;
	}

	pfrom = dev->core[row][col].mems.base + from_addr;
	diag(H_D2) { fprintf(diag_fd, "ee_read_buf(): reading from from_addr=0x%08x, pfrom=0x%08x, size=%d\n", (uint) from_addr, (uint) pfrom, (int) size); }

	if ((dev->type == E_E64G401) && ((row >= 1) && (row <= 2)))
	{
		// The following code is a fix for the E64G401 anomaly of bursting reads from eCore
		// internal memory back to host, from rows #1 and #2.
		addr_from = (unsigned int) pfrom;
		addr_to	  = (unsigned int) buf;
		align	  = (addr_from | addr_to | size) & 0x7;

		switch (align) {
		case 0x0:
			for (i=0; i<size; i+=sizeof(int64_t))
				*((int64_t *) (buf + i)) = *((int64_t *) (pfrom + i));
			break;
		case 0x1:
		case 0x3:
		case 0x5:
		case 0x7:
			for (i=0; i<size; i+=sizeof(int8_t))
				*((int8_t  *) (buf + i)) = *((int8_t  *) (pfrom + i));
			break;
		case 0x2:
		case 0x6:
			for (i=0; i<size; i+=sizeof(int16_t))
				*((int16_t *) (buf + i)) = *((int16_t *) (pfrom + i));
			break;
		case 0x4:
			for (i=0; i<size; i+=sizeof(int32_t))
				*((int32_t *) (buf + i)) = *((int32_t *) (pfrom + i));
			break;
		}
	}
	else
		memcpy(buf, pfrom, size);

	return size;
}


// Write a memory block to SRAM of a core in a group
ssize_t ee_write_buf(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, const void *buf, size_t size)
{
	void *pto;

	if (((to_addr + size) > dev->core[row][col].mems.map_size) || (to_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_write_buf(): writing to to_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) to_addr, (uint) size, (uint) dev->core[row][col].mems.map_size); }
		warnx("ee_write_buf(): Buffer range is out of bounds.");
		return E_ERR;
	}

	pto = dev->core[row][col].mems.base + to_addr;
	diag(H_D2) { fprintf(diag_fd, "ee_write_buf(): writing to to_addr=0x%08x, pto=0x%08x, size=%d\n", (uint) to_addr, (uint) pto, (int) size); }
	memcpy(pto, buf, size);

	return size;
}


// Read a core register from a core in a group
int ee_read_reg(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr)
{
	volatile int *pfrom;
	off_t		  addr;
	int			  data;
	ssize_t		  size;

	addr = from_addr;
	if (addr >= E_CORE_GP_REG_BASE)
		addr = addr - E_CORE_GP_REG_BASE;

	size = sizeof(int);
	if (((addr + size) > dev->core[row][col].regs.map_size) || (addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_read_reg(): from_addr=0x%08x, size=0x%08x, map_size=0x%08x\n", (uint) from_addr, (uint) size, (uint) dev->core[row][col].regs.map_size); }
		warnx("ee_read_reg(): Address is out of bounds.");
		return E_ERR;
	}

	pfrom = (int *) (dev->core[row][col].regs.base + addr);
	diag(H_D2) { fprintf(diag_fd, "ee_read_reg(): reading from from_addr=0x%08x, pfrom=0x%08x\n", (uint) from_addr, (uint) pfrom); }
	data  = *pfrom;

	return data;
}


// Write to a core register of a core in a group
ssize_t ee_write_reg(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, int data)
{
	int		*pto;
	ssize_t	 size;

	if (to_addr >= E_CORE_GP_REG_BASE)
		to_addr = to_addr - E_CORE_GP_REG_BASE;

	size = sizeof(int);
	if (((to_addr + size) > dev->core[row][col].regs.map_size) || (to_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_write_reg(): writing to to_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) to_addr, (uint) size, (uint) dev->core[row][col].regs.map_size); }
		warnx("ee_write_reg(): Address is out of bounds.");
		return E_ERR;
	}

	pto = (int *) (dev->core[row][col].regs.base + to_addr);
	diag(H_D2) { fprintf(diag_fd, "ee_write_reg(): writing to to_addr=0x%08x, pto=0x%08x\n", (uint) to_addr, (uint) pto); }
	*pto = data;

	return sizeof(int);
}

// External Memory access
//
// Allocate a buffer in external memory
int e_alloc(e_mem_t *mbuf, off_t offset, size_t size)
{
	if (e_platform.initialized == E_FALSE)
	{
		warnx("e_alloc(): Platform was not initialized. Use e_init().");
		return E_ERR;
	}

	mbuf->objtype = E_EXT_MEM;

	mbuf->memfd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
	if (mbuf->memfd == -1)
	{
		warnx("e_alloc(): EPIPHANY_DEV file open failure.");
		return E_ERR;
	}

	diag(H_D2) { fprintf(diag_fd, "e_alloc(): allocating EMEM buffer at offset 0x%08x\n", (uint) offset); }

	mbuf->phy_base = (e_platform.emem[0].phy_base + offset); // TODO: this takes only the 1st segment into account
	mbuf->page_base = ee_rndl_page(mbuf->phy_base);
	mbuf->page_offset = mbuf->phy_base - mbuf->page_base;
	mbuf->map_size = size + mbuf->page_offset;

	mbuf->mapped_base = mmap(NULL, mbuf->map_size, PROT_READ|PROT_WRITE, MAP_SHARED, mbuf->memfd, mbuf->page_base);
	mbuf->base = (void*)(((char*)mbuf->mapped_base) + mbuf->page_offset);

	mbuf->ephy_base = (e_platform.emem[0].ephy_base + offset); // TODO: this takes only the 1st segment into account
	mbuf->emap_size = size;

	diag(H_D2) { fprintf(diag_fd, "e_alloc(): mbuf.phy_base = 0x%08x, mbuf.ephy_base = 0x%08x, mbuf.base = 0x%08x, mbuf.size = 0x%08x\n", (uint) mbuf->phy_base, (uint) mbuf->ephy_base, (uint) mbuf->base, (uint) mbuf->map_size); }

	if (mbuf->mapped_base == MAP_FAILED)
	{
		warnx("e_alloc(): mmap failure.");
		return E_ERR;
	}

	return E_OK;
}

// Free a memory buffer in external memory
int e_free(e_mem_t *mbuf)
{
	if (NULL == mbuf) {
		return E_ERR;
	}

	if ( E_SHARED_MEM == mbuf->objtype ) {
		// The shared memory mapping is persistent - don't unmap
		munmap(mbuf->mapped_base, mbuf->map_size);
		close(mbuf->memfd);
	}

	return E_OK;
}


// Read a block from an external memory buffer
ssize_t ee_mread(e_mem_t *mbuf, const off_t from_addr, void *buf, size_t size)
{
	ssize_t rcount;

	rcount = ee_mread_buf(mbuf, from_addr, buf, size);

	return rcount;
}


// Write a block to an external memory buffer
ssize_t ee_mwrite(e_mem_t *mbuf, off_t to_addr, const void *buf, size_t size)
{
	ssize_t wcount;

	wcount = ee_mwrite_buf(mbuf, to_addr, buf, size);

	return wcount;
}


// Read a word from an external memory buffer
int ee_mread_word(e_mem_t *mbuf, const off_t from_addr)
{
	volatile int *pfrom;
	int			  data;
	ssize_t		  size;

	size = sizeof(int);
	if (((from_addr + size) > mbuf->map_size) || (from_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_mread_word(): writing to from_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) from_addr, (uint) size, (uint) mbuf->map_size); }
		warnx("ee_mread_word(): Address is out of bounds.");
		return E_ERR;
	}

	pfrom = (int *) (mbuf->base + from_addr);
	diag(H_D2) { fprintf(diag_fd, "ee_mread_word(): reading from from_addr=0x%08x, pfrom=0x%08x\n", (uint) from_addr, (uint) pfrom); }
	data  = *pfrom;

	return data;
}


// Write a word to an external memory buffer
ssize_t ee_mwrite_word(e_mem_t *mbuf, off_t to_addr, int data)
{
	int		*pto;
	ssize_t	 size;

	size = sizeof(int);
	if (((to_addr + size) > mbuf->map_size) || (to_addr < 0))
	{
		diag(H_D2) { fprintf(diag_fd, "ee_mwrite_word(): writing to to_addr=0x%08x, size=%d, map_size=0x%x\n", (uint) to_addr, (uint) size, (uint) mbuf->map_size); }
		warnx("ee_mwrite_word(): Address is out of bounds.");
		return E_ERR;
	}

	pto = (int *) (mbuf->base + to_addr);
	diag(H_D2) { fprintf(diag_fd, "ee_mwrite_word(): writing to to_addr=0x%08x, pto=0x%08x\n", (uint) to_addr, (uint) pto); }
	*pto = data;

	return sizeof(int);
}


// Read a block from an external memory buffer
ssize_t ee_mread_buf(e_mem_t *mbuf, const off_t from_addr, void *buf, size_t size)
{
	const void *pfrom;

	if (((from_addr + size) > mbuf->map_size) || (from_addr < 0))
	{
		warnx("ee_mread_buf(): Address is out of bounds.");
		return E_ERR;
	}

	pfrom = mbuf->base + from_addr;

	diag(H_D1) {
		fprintf(diag_fd, "ee_mread_buf(): reading from from_addr=0x%08x, "
				"offset=0x%08x, size=%d, map_size=0x%x\n", (uint) pfrom,
				(uint)from_addr, (uint) size, (uint) mbuf->map_size);
	}

	memcpy(buf, pfrom, size);

	return size;
}


// Write a block to an external memory buffer
ssize_t ee_mwrite_buf(e_mem_t *mbuf, off_t to_addr, const void *buf, size_t size)
{
	void *pto;

	if (((to_addr + size) > mbuf->map_size) || (to_addr < 0))
	{
		warnx("ee_mwrite_buf(): Address is out of bounds.");
		return E_ERR;
	}

	pto = mbuf->base + to_addr;

	if ( E_SHARED_MEM == mbuf->objtype ) {
		diag(H_D1) {
		  fprintf(diag_fd, "ee_mwrite_buf(): writing to to_addr=0x%08x, "
				  "offset=0x%08x, size=%d, map_size=0x%x\n", (uint) pto,
				  (uint)to_addr, (uint) size, (uint) mbuf->map_size);
		}
	}
	memcpy(pto, buf, size);

	return size;
}


//////////////////
// Platform access
//
// Read a word from an address in the platform space
int ee_read_esys(off_t from_addr)
{
	e_mmap_t	  esys;
	int			  memfd;
	volatile int *pfrom;
	int			  data;

	// Open memory device
	memfd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
	if (memfd == -1)
	{
		warnx("ee_read_esys(): EPIPHANY_DEV file open failure.");
		return E_ERR;
	}

	esys.phy_base = (e_platform.regs_base + from_addr);
	esys.page_base = ee_rndl_page(esys.phy_base);
	esys.page_offset = esys.phy_base - esys.page_base;
	esys.map_size = sizeof(int) + esys.page_offset;

	esys.mapped_base = mmap(NULL, esys.map_size, PROT_READ|PROT_WRITE, MAP_SHARED, memfd, esys.page_base);
	esys.base = esys.mapped_base + esys.page_offset;

	diag(H_D2) { fprintf(diag_fd, "ee_read_esys(): esys.phy_base = 0x%08x, esys.base = 0x%08x, esys.size = 0x%08x\n", (uint) esys.phy_base, (uint) esys.base, (uint) esys.map_size); }

	if (esys.mapped_base == MAP_FAILED)
	{
		warnx("ee_read_esys(): ESYS mmap failure.");
		return E_ERR;
	}

	pfrom = (int *) (esys.base);
	diag(H_D2) { fprintf(diag_fd, "ee_read_esys(): reading from from_addr=0x%08x, pto=0x%08x\n", (uint) from_addr, (uint) pfrom); }
	data  = *pfrom;

	munmap(esys.mapped_base, esys.map_size);
	close(memfd);

	return data;
}


// Write a word to an address in the platform space
ssize_t ee_write_esys(off_t to_addr, int data)
{
	e_mmap_t  esys;
	int		  memfd;
	int		 *pto;

	// Open memory device
	memfd = open(EPIPHANY_DEV, O_RDWR | O_SYNC);
	if (memfd == -1)
	{
		warnx("ee_write_esys(): EPIPHANY_DEV file open failure.");
		return E_ERR;
	}

	esys.phy_base = (e_platform.regs_base + to_addr);
	esys.page_base = ee_rndl_page(esys.phy_base);
	esys.page_offset = esys.phy_base - esys.page_base;
	esys.map_size = sizeof(int) + esys.page_offset;

	esys.mapped_base = mmap(NULL, esys.map_size, PROT_READ|PROT_WRITE, MAP_SHARED, memfd, esys.page_base);
	esys.base = esys.mapped_base + esys.page_offset;

	diag(H_D2) { fprintf(diag_fd, "ee_write_esys(): esys.phy_base = 0x%08x, esys.page_base = 0x%08x, esys.page_offset = 0x%08x, esys.base = 0x%08x, esys.size = 0x%08x\n", (uint) esys.phy_base, (uint) esys.page_base, (uint) esys.page_offset, (uint) esys.base, (uint) esys.map_size); }

	if (esys.mapped_base == MAP_FAILED)
	{
		warnx("ee_write_esys(): ESYS mmap failure.");
		return E_ERR;
	}

	pto = (int *) (esys.base);
	diag(H_D2) { fprintf(diag_fd, "ee_write_esys(): writing to to_addr=0x%08x, pto=0x%08x\n", (uint) (e_platform.regs_base + to_addr), (uint) pto); }
	*pto = data;

	munmap(esys.mapped_base, esys.map_size);
	close(memfd);

	return sizeof(int);
}



/////////////////////////
// Core control functions
//
// Reset the Epiphany platform
int e_reset_system(void)
{
  unsigned int data;
  diag(H_D1) { fprintf(diag_fd, "e_reset_system(): resetting full ESYS...\n"); }
  
  //Turn on chip clock to low speed
  ee_write_esys(E_REG_ESYSCFGCLK,0x1);
  
  //Bring into reset
  ee_write_esys(E_REG_SYSRESET, 0x1);	
  
  //Turn off chip clock (for Errata workaround)
  ee_write_esys(E_REG_ESYSCFGCLK,0x0);
  
  //Bring Epiphany out of reset
  ee_write_esys(E_REG_SYSRESET, 0x0);	
  
  //Turn on clock to Epiphany (to default speed)
  ee_write_esys(E_REG_ESYSCFGCLK,0x4);
  
  //Turn on Host eLink Transmitter
  ee_write_esys(E_REG_ESYSCFGTX,0x1);
  
  //Turn on Host eLink Receiver
  ee_write_esys(E_REG_ESYSCFGRX,0x1);
  
  //Turn on clock gating mode in Epiphany
  e_open(&dev, 0, 0, platform.rows, platform.cols);       
  for (i=0; i<platform.rows; i++) {
    for (j=0; j<platform.cols; j++) {
      //eCore clock gating
      data=0x00400000;
      e_write(&dev, i, j, E_REG_CONFIG, &data, sizeof(data));
      //eMesh clock gating
      data=0x00000002;
      e_write(&dev, i, j, E_REG_MESHCONFIG, &data, sizeof(data));
    }
  }
  
        // Close the workgroup
        e_close(&dev);


	diag(H_D1) { fprintf(diag_fd, "e_reset_system(): done.\n"); }

	return E_OK;
}


// Reset the Epiphany chip
int e_reset_chip(void)
{
	diag(H_D1) { fprintf(diag_fd, "e_reset_chip(): This operation is not implemented!\n"); }

	return E_OK;
}


// Reset an e-core
int ee_reset_core(e_epiphany_t *dev, unsigned row, unsigned col)
{
	int RESET0 = 0x0;
	int RESET1 = 0x1;
	int CONFIG = 0x01000000;

	diag(H_D1) { fprintf(diag_fd, "e_reset_core(): halting core (%d,%d) (0x%03x)...\n", row, col, dev->core[row][col].id); }
	e_halt(dev, row, col);

	diag(H_D1) { fprintf(diag_fd, "e_reset_core(): pausing DMAs.\n"); }
	e_write(dev, row, col, E_REG_CONFIG, &CONFIG, sizeof(unsigned));

	usleep(100000);

	diag(H_D1) { fprintf(diag_fd, "e_reset_core(): resetting core (%d,%d) (0x%03x)...\n", row, col, dev->core[row][col].id); }
	ee_write_reg(dev, row, col, E_REG_RESETCORE, RESET1);
	ee_write_reg(dev, row, col, E_REG_RESETCORE, RESET0);
	diag(H_D1) { fprintf(diag_fd, "e_reset_core(): done.\n"); }

	return E_OK;
}


// Reset a workgroup
int e_reset_group(e_epiphany_t *dev)
{
	int RESET0 = 0x0;
	int RESET1 = 0x1;
	int CONFIG = 0x01000000;
	int row, col;

	diag(H_D1) { fprintf(diag_fd, "e_reset_group(): halting core...\n"); }
	for (row=0; row<dev->rows; row++)
		for (col=0; col<dev->cols; col++)
			e_halt(dev, row, col);
	diag(H_D1) { fprintf(diag_fd, "e_reset_group(): pausing DMAs.\n"); }

	for (row=0; row<dev->rows; row++)
		for (col=0; col<dev->cols; col++)
			e_write(dev, row, col, E_REG_CONFIG, &CONFIG, sizeof(unsigned));

	usleep(100000);

	diag(H_D1) { fprintf(diag_fd, "e_reset_group(): resetting cores...\n"); }
	for (row=0; row<dev->rows; row++)
		for (col=0; col<dev->cols; col++)
		{
			ee_write_reg(dev, row, col, E_REG_RESETCORE, RESET1);
			ee_write_reg(dev, row, col, E_REG_RESETCORE, RESET0);
		}

	diag(H_D1) { fprintf(diag_fd, "e_reset_group(): done.\n"); }

	return E_OK;
}


// Start a program loaded on an e-core in a group
int e_start(e_epiphany_t *dev, unsigned row, unsigned col)
{
	e_return_stat_t retval;

	retval = E_OK;

	int SYNC = (1 << E_SYNC);

	diag(H_D1) { fprintf(diag_fd, "e_start(): SYNC (0x%x) to core (%d,%d)...\n", E_REG_ILATST, row, col); }
	if (ee_write_reg(dev, row, col, E_REG_ILATST, SYNC) == E_ERR)
		retval = E_ERR;
	diag(H_D1) { fprintf(diag_fd, "e_start(): done.\n"); }

	return retval;
}


// Start all programs loaded on a workgroup
int e_start_group(e_epiphany_t *dev)
{
	int				irow, icol;
	e_return_stat_t retval;

	retval = E_OK;

	int SYNC = (1 << E_SYNC);

	diag(H_D1) { fprintf(diag_fd, "e_start_group(): SYNC (0x%x) to workgroup...\n", E_REG_ILATST); }
	for (irow=0; irow<dev->rows; irow++)
		for (icol=0; icol<dev->cols; icol++)
			{
				diag(H_D1) { fprintf(diag_fd, "e_start_group(): send SYNC signal to core (%d,%d)...\n", irow, icol); }
				if (ee_write_reg(dev, irow, icol, E_REG_ILATST, SYNC) == E_ERR)
					retval = E_ERR;
			}
	diag(H_D1) { fprintf(diag_fd, "e_start_group(): done.\n"); }

	return retval;
}


// Signal a software interrupt to an e-core in a group
int e_signal(e_epiphany_t *dev, unsigned row, unsigned col)
{
	int SWI = (1 << E_USER_INT);

	diag(H_D1) { fprintf(diag_fd, "e_signal(): SWI (0x%x) to core (%d,%d)...\n", E_REG_ILATST, row, col); }
	ee_write_reg(dev, row, col, E_REG_ILATST, SWI);
	diag(H_D1) { fprintf(diag_fd, "e_signal(): done.\n"); }

	return E_OK;
}


// Halt a core
int e_halt(e_epiphany_t *dev, unsigned row, unsigned col)
{
	int cmd;

	cmd = 0x1;
	e_write(dev, row, col, E_REG_DEBUGCMD, &cmd, sizeof(int));

	return E_OK;
}


// Resume a core after halt
int e_resume(e_epiphany_t *dev, unsigned row, unsigned col)
{
	int cmd;

	cmd = 0x0;
	e_write(dev, row, col, E_REG_DEBUGCMD, &cmd, sizeof(int));

	return E_OK;
}


////////////////////
// Utility functions (not public)

// Convert core coordinates to core-number within a group. No bounds check is performed.
unsigned e_get_num_from_coords(e_epiphany_t *dev, unsigned row, unsigned col)
{
	unsigned corenum;

	corenum = col + row * dev->cols;
	diag(H_D2) { fprintf(diag_fd, "e_get_num_from_coords(): dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d), corenum=%d\n", dev->row, dev->col, dev->rows, dev->cols, row, col, corenum); }

	return corenum;
}


// Convert CoreID to core-number within a group.
unsigned ee_get_num_from_id(e_epiphany_t *dev, unsigned coreid)
{
	unsigned row, col, corenum;

	row = (coreid >> 6) & 0x3f;
	col = (coreid >> 0) & 0x3f;
	corenum = (col - dev->col) + (row - dev->row) * dev->cols;
	diag(H_D2) { fprintf(diag_fd, "ee_get_num_from_id(): CoreID=0x%03x, dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d), corenum=%d\n", coreid, dev->row, dev->col, dev->rows, dev->cols, row, col, corenum); }

	return corenum;
}


// Converts core coordinates to CoreID.
unsigned ee_get_id_from_coords(e_epiphany_t *dev, unsigned row, unsigned col)
{
	unsigned coreid;

	coreid = (dev->col + col) + ((dev->row + row) << 6);
	diag(H_D2) { fprintf(diag_fd, "ee_get_id_from_coords(): dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d), CoreID=0x%03x\n", dev->row, dev->col, dev->rows, dev->cols, row, col, coreid); }

	return coreid;
}


// Converts core-number within a group to CoreID.
unsigned ee_get_id_from_num(e_epiphany_t *dev, unsigned corenum)
{
	unsigned row, col, coreid;

	row = corenum / dev->cols;
	col = corenum % dev->cols;
	coreid = (dev->col + col) + ((dev->row + row) << 6);
	diag(H_D2) { fprintf(diag_fd, "ee_get_id_from_num(): corenum=%d, dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d), CoreID=0x%03x\n", corenum, dev->row, dev->col, dev->rows, dev->cols, row, col, coreid); }

	return coreid;
}


// Converts CoreID to core coordinates.
void ee_get_coords_from_id(e_epiphany_t *dev, unsigned coreid, unsigned *row, unsigned *col)
{
	*row = ((coreid >> 6) & 0x3f) - dev->row;
	*col = ((coreid >> 0) & 0x3f) - dev->col;
	diag(H_D2) { fprintf(diag_fd, "ee_get_coords_from_id(): CoreID=0x%03x, dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d)\n", coreid, dev->row, dev->col, dev->rows, dev->col, *row, *col); }

	return;
}

// Converts core-number within a group to core coordinates.
void e_get_coords_from_num(e_epiphany_t *dev, unsigned corenum, unsigned *row, unsigned *col)
{
	*row = corenum / dev->cols;
	*col = corenum % dev->cols;
	diag(H_D2) { fprintf(diag_fd, "e_get_coords_from_num(): corenum=%d, dev.(row,col,rows,cols)=(%d,%d,%d,%d), (row,col)=(%d,%d)\n", corenum, dev->row, dev->col, dev->col, dev->rows, *row, *col); }

	return;
}


// Check if an address is on a chip region
e_bool_t e_is_addr_on_chip(void *addr)
{
	unsigned  row, col;
	unsigned  coreid, i;
	e_chip_t *curr_chip;

	coreid = ((unsigned) addr) >> 20;
	row = (coreid >> 6) & 0x3f;
	col = (coreid >> 0) & 0x3f;

	for (i=0; i<e_platform.num_chips; i++)
	{
		curr_chip = &(e_platform.chip[i]);
		if ((row >= curr_chip->row) && (row < (curr_chip->row + curr_chip->rows)) &&
			(col >= curr_chip->col) && (col < (curr_chip->col + curr_chip->cols)))
			return E_TRUE;
	}

	return E_FALSE;
}


// Check if an address is on a core-group region
e_bool_t e_is_addr_on_group(e_epiphany_t *dev, void *addr)
{
	unsigned row, col;
	unsigned coreid;

	coreid = ((unsigned) addr) >> 20;
	ee_get_coords_from_id(dev, coreid, &row, &col);

	if ((row < dev->rows) && (col < dev->cols))
		return E_TRUE;

	return E_FALSE;
}

e_hal_diag_t e_set_host_verbosity(e_hal_diag_t verbose)
{
	e_hal_diag_t old_host_verbose;

	old_host_verbose = e_host_verbose;
	diag_fd = stderr;
	e_host_verbose = verbose;

	return old_host_verbose;
}



////////////////////////////////////
// HDF parser

int ee_parse_hdf(e_platform_t *dev, char *hdf)
{
	int	  ret = E_ERR;
	char *ext;

	if (strlen(hdf) >= 4)
	{
		ext = hdf + strlen(hdf) - 4;
		if (!strcmp(ext, ".hdf"))
			ret = ee_parse_simple_hdf(dev, hdf);
		else if (!strcmp(ext, ".xml"))
			ret = ee_parse_xml_hdf(dev, hdf);
		else
			ret = E_ERR;
	} else {
		ret = E_ERR;
	}

	return ret;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
int ee_parse_simple_hdf(e_platform_t *dev, char *hdf)
{
	FILE	   *fp;
	int			chip_num;
	int			emem_num;
	e_chip_t   *curr_chip = NULL;
	e_memseg_t *curr_emem = NULL;

	char line[255], etag[255], eval[255], *dummy;
	int l;

	fp = fopen(hdf, "r");
	if (fp == NULL)
	{
		warnx("ee_parse_simple_hdf(): Can't open Hardware Definition File (HDF) %s.", hdf);
		return E_ERR;
	}

	chip_num = -1;
	emem_num = -1;

	l = 0;

	while (!feof(fp))
	{
		l++;
		dummy = fgets(line, sizeof(line), fp);
		ee_trim_str(line);
		if (!strcmp(line, ""))
			continue;
		sscanf(line, "%s %s", etag, eval);
		diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): line %d: %s %s\n", l, etag, eval); }


		// Platform definition
		if		(!strcmp("PLATFORM_VERSION", etag))
		{
			sscanf(eval, "%s", dev->version);
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): platform version = %s\n", dev->version); }
		}

		else if (!strcmp("NUM_CHIPS", etag))
		{
			sscanf(eval, "%d", &(dev->num_chips));
			dev->chip = (e_chip_t *) calloc(dev->num_chips, sizeof(e_chip_t));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): number of chips = %d\n", dev->num_chips); }
		}

		else if (!strcmp("NUM_EXT_MEMS", etag))
		{
			sscanf(eval, "%d", &(dev->num_emems));
			dev->emem = (e_memseg_t *) calloc(dev->num_emems, sizeof(e_memseg_t));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): number of ext. memory segments = %d\n", dev->num_emems); }
		}

		else if (!strcmp("ESYS_REGS_BASE", etag))
		{
			sscanf(eval, "%x", &(dev->regs_base));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): base address of platform registers = 0x%08x\n", dev->regs_base); }
		}


		// Chip definition
		else if (!strcmp("CHIP", etag))
		{
			chip_num++;
			curr_chip = &(dev->chip[chip_num]);
			sscanf(eval, "%s", curr_chip->version);
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): processing chip #%d, version = \"%s\"\n", chip_num, curr_chip->version); }
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): chip version = %s\n", curr_chip->version); }
		}

		else if (!strcmp("CHIP_ROW", etag))
		{
			sscanf(eval, "%d", &(curr_chip->row));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): chip row = %d\n", curr_chip->row); }
		}

		else if (!strcmp("CHIP_COL", etag))
		{
			sscanf(eval, "%d", &(curr_chip->col));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): chip col = %d\n", curr_chip->col); }
		}


		// External memory definitions
		else if (!strcmp("EMEM", etag))
		{
			emem_num++;
			curr_emem = &(dev->emem[emem_num]);
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): processing external memory segment #%d\n", emem_num); }
		}

		else if (!strcmp("EMEM_BASE_ADDRESS", etag))
		{
			sscanf(eval, "%x", (unsigned int *) &(curr_emem->phy_base));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): base addr. of ext. mem. segment = 0x%08x\n", (uint) curr_emem->phy_base); }
		}

		else if (!strcmp("EMEM_EPI_BASE", etag))
		{
			sscanf(eval, "%x", (unsigned int *) &(curr_emem->ephy_base));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): base addr. of ext. mem. segment (device side)= 0x%08x\n", (uint) curr_emem->ephy_base); }
		}

		else if (!strcmp("EMEM_SIZE", etag))
		{
			sscanf(eval, "%x", (unsigned int *) &(curr_emem->size));
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): size of ext. mem. segment = %x\n", (uint) curr_emem->size); }
		}

		else if (!strcmp("EMEM_TYPE", etag))
		{
			if (!strcmp(etag, "RD"))
				curr_emem->type = E_RD;
			else if (!strcmp(etag, "WR"))
				curr_emem->type = E_WR;
			else if (!strcmp(etag, "RDWR"))
				curr_emem->type = E_RDWR;
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): type of ext. mem. segment = %x\n", (uint) curr_emem->type); }
		}


		// Other
		else if (!strcmp("//", etag))
		{
			;
			diag(H_D3) { fprintf(diag_fd, "ee_parse_simple_hdf(): comment\n"); }
		}
		else {
			return E_ERR;
		}
	}

	fclose(fp);

	return E_OK;
}
#pragma GCC diagnostic pop

int ee_parse_xml_hdf(e_platform_t *dev, char *hdf)
{
	(void)dev;
	(void)hdf;
	warnx("ee_parse_xml_hdf(): XML file format is not yet supported. "
		  "Please use simple HDF format.");

	return E_ERR;
}


// Platform data structures
typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	e_platformtype_t type;		  // Epiphany platform part number
	char			 version[32]; // version name of Epiphany chip
} e_platform_db_t;

#define NUM_PLATFORM_VERSIONS 7
e_platform_db_t platform_params_table[NUM_PLATFORM_VERSIONS] = {
//		 objtype		 type			   version
		{E_EPI_PLATFORM, E_GENERIC,		   "GENERIC"},
		{E_EPI_PLATFORM, E_EMEK301,		   "EMEK301"},
		{E_EPI_PLATFORM, E_EMEK401,		   "EMEK401"},
		{E_EPI_PLATFORM, E_ZEDBOARD1601,   "ZEDBOARD1601"},
		{E_EPI_PLATFORM, E_ZEDBOARD6401,   "ZEDBOARD6401"},
		{E_EPI_PLATFORM, E_PARALLELLA1601, "PARALLELLA1601"},
		{E_EPI_PLATFORM, E_PARALLELLA6401, "PARALLELLA6401"},
};


int ee_set_platform_params(e_platform_t *platform)
{
	int platform_ver;

	for (platform_ver = 0; platform_ver < NUM_PLATFORM_VERSIONS; platform_ver++)
		if (!strcmp(platform->version, platform_params_table[platform_ver].version))
		{
			diag(H_D2) { fprintf(diag_fd, "ee_set_platform_params(): found platform version \"%s\"\n", platform->version); }
			break;
		}

	if (platform_ver == NUM_PLATFORM_VERSIONS)
	{
		diag(H_D2) { fprintf(diag_fd, "ee_set_platform_params(): platform version \"%s\" not found, setting to \"%s\" type\n", platform->version, platform_params_table[0].version); }
		platform_ver = 0;
	}

	platform->type = platform_params_table[platform_ver].type;

	return E_OK;
}


typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	e_chiptype_t	 type;		  // Epiphany chip part number
	char			 version[32]; // version name of Epiphany chip
	unsigned int	 arch;		  // architecture generation
	unsigned int	 rows;		  // number of rows in chip
	unsigned int	 cols;		  // number of cols in chip
	unsigned int	 sram_base;	  // base offset of core SRAM
	unsigned int	 sram_size;	  // size of core SRAM
	unsigned int	 regs_base;	  // base offset of core registers
	unsigned int	 regs_size;	  // size of core registers segment
	off_t			 ioregs_n;	  // base address of north IO register
	off_t			 ioregs_e;	  // base address of east IO register
	off_t			 ioregs_s;	  // base address of south IO register
	off_t			 ioregs_w;	  // base address of west IO register
} e_chip_db_t;

#define NUM_CHIP_VERSIONS 2
e_chip_db_t chip_params_table[NUM_CHIP_VERSIONS] = {
//		 objtype	 type		version	 arch r	 c sram_base sram_size regs_base regs_size io_n		io_e		io_s		io_w
		{E_EPI_CHIP, E_E16G301, "E16G301", 3, 4, 4, 0x00000, 0x08000, 0xf0000, 0x01000, 0x002f0000, 0x083f0000, 0x0c2f0000, 0x080f0000},
		{E_EPI_CHIP, E_E64G401, "E64G401", 4, 8, 8, 0x00000, 0x08000, 0xf0000, 0x01000, 0x002f0000, 0x087f0000, 0x1c2f0000, 0x080f0000},
};


int ee_set_chip_params(e_chip_t *chip)
{
	int chip_ver;

	for (chip_ver = 0; chip_ver < NUM_CHIP_VERSIONS; chip_ver++)
		if (!strcmp(chip->version, chip_params_table[chip_ver].version))
		{
			diag(H_D2) { fprintf(diag_fd, "ee_set_chip_params(): found chip version \"%s\"\n", chip->version); }
			break;
		}

	if (chip_ver == NUM_CHIP_VERSIONS)
	{
		diag(H_D2) { fprintf(diag_fd, "ee_set_chip_params(): chip version \"%s\" not found, setting to \"%s\"\n", chip->version, chip_params_table[0].version); }
		chip_ver = 0;
	}

	chip->type		= chip_params_table[chip_ver].type;
	chip->arch		= chip_params_table[chip_ver].arch;
	chip->rows		= chip_params_table[chip_ver].rows;
	chip->cols		= chip_params_table[chip_ver].cols;
	chip->num_cores = chip->rows * chip->cols;
	chip->sram_base = chip_params_table[chip_ver].sram_base;
	chip->sram_size = chip_params_table[chip_ver].sram_size;
	chip->regs_base = chip_params_table[chip_ver].regs_base;
	chip->regs_size = chip_params_table[chip_ver].regs_size;
	chip->ioregs_n	= chip_params_table[chip_ver].ioregs_n;
	chip->ioregs_e	= chip_params_table[chip_ver].ioregs_e;
	chip->ioregs_s	= chip_params_table[chip_ver].ioregs_s;
	chip->ioregs_w	= chip_params_table[chip_ver].ioregs_w;

	return E_OK;
}


void ee_trim_str(char *a)
{
	char *b = a;
	while (isspace(*b))	  ++b;
	while (*b)			  *a++ = *b++;
	*a = '\0';
	while (isspace(*--a)) *a = '\0';
}


unsigned long ee_rndu_page(unsigned long size)
{
	unsigned long page_size;
	unsigned long rsize;

	// Get OS memory page size
	page_size = sysconf(_SC_PAGE_SIZE);

	// Find upper integral number of pages
	rsize = ((size + (page_size - 1)) / page_size) * page_size;

	return rsize;
}


unsigned long ee_rndl_page(unsigned long size)
{
	unsigned long page_size;
	unsigned long rsize;

	// Get OS memory page size
	page_size = sysconf(_SC_PAGE_SIZE);

	// Find lower integral number of pages
	rsize = (size / page_size) * page_size;

	return rsize;
}

#pragma GCC diagnostic pop
