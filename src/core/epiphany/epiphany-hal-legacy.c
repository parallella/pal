

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <err.h>
#include <stdio.h>
#include <stdlib.h>

#include "e-hal.h"
#include "epiphany-hal-api-local.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

#define diag(vN)   if (e_host_verbose >= vN)

extern int   e_host_verbose;
extern FILE *diag_fd;

extern e_platform_t e_platform;

////////////////////////////////////
// ftdi_target wrapper functionality
#include <e-xml/src/epiphany_platform.h>

static e_epiphany_t Epiphany, *pEpiphany;
static e_mem_t      ERAM,     *pERAM;

platform_definition_t *platform;


// Absolute global memory access TODO: needs review!!!
ssize_t ee_read_abs(unsigned address, void *buf, size_t burst_size)
{
	ssize_t  rcount;
	e_bool_t isglobal, isexternal, isonchip, isregs, ismems;
	unsigned coreid;
	unsigned row, col, i;

	diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): address = 0x%08x\n", address); }
	isglobal = ((address & 0xfff00000) != 0) ? E_TRUE : E_FALSE;
	if (isglobal)
	{
		if (((address >= pERAM->phy_base)  && (address < (pERAM->phy_base + pERAM->map_size))) ||
		    ((address >= pERAM->ephy_base) && (address < (pERAM->ephy_base + pERAM->emap_size))))
		{
			isexternal = E_TRUE;
		} else {
			isexternal = E_FALSE;
			coreid     = address >> 20;
			isonchip   = e_is_addr_on_chip((void *) address);
			if (isonchip)
			{
				ee_get_coords_from_id(pEpiphany, coreid, &row, &col);
				ismems  = (address <  pEpiphany->core[row][col].mems.phy_base + pEpiphany->core[row][col].mems.map_size);
				isregs  = (address >= pEpiphany->core[row][col].regs.phy_base);
			}
		}
	}

	if (isglobal)
	{
		if (isexternal)
		{
			diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): to external memory.\n"); }
			if ((address >= pERAM->ephy_base) && (address < (pERAM->ephy_base + pERAM->emap_size)))
			{
				diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): converting virtual (0x%08x) ", address); }
				address = address - (pERAM->ephy_base - pERAM->phy_base);
				diag(H_D2) { fprintf(diag_fd, "to physical (0x%08x)...\n", address); }
			}
			diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): converting physical (0x%08x) ", address); }
			address = address - pERAM->phy_base;
			diag(H_D2) { fprintf(diag_fd, "to offset (0x%08x)...\n", address); }
			rcount = ee_mread_buf(pERAM, address, buf, burst_size);
			diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): isexternal -> rcount = %d\n", (int) rcount); }
		} else if (isonchip)
		{
			address = address & 0x000fffff;
			if (ismems) {
				rcount = ee_read_buf(pEpiphany, row, col, address, buf, burst_size);
				diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): isonchip/ismems -> rcount = %d\n", (int) rcount); }
			} else if (isregs) {
				for (rcount=0, i=0; i<burst_size; i+=sizeof(unsigned)) {
					*((unsigned *) (buf+i)) = ee_read_reg(pEpiphany, row, col, (address+i));
					rcount += sizeof(unsigned);
				}
				diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): isonchip/isregs -> rcount = %d\n", (int) rcount); }
			} else {
				rcount = 0;
				diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): is a reserved on-chip address -> rcount = %d\n", (int) rcount); }
			}
		} else {
			rcount = 0;
			diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): is not a legal address -> rcount = %d\n", (int) rcount); }
		}
	} else {
		rcount = 0;
		diag(H_D1) { fprintf(diag_fd, "ee_read_abs(): is not a global address -> rcount = %d\n", (int) rcount); }
	}

	return rcount;
}


// TODO: needs review!!!
ssize_t ee_write_abs(unsigned address, void *buf, size_t burst_size)
{
	ssize_t  rcount;
	unsigned isglobal, isexternal, isonchip, isregs, ismems;
	unsigned coreid;
	unsigned row, col, i;

	diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): address = 0x%08x\n", address); }
	isglobal = ((address & 0xfff00000) != 0) ? E_TRUE : E_FALSE;
	if (isglobal)
	{
		if (((address >= pERAM->phy_base)  && (address < (pERAM->phy_base + pERAM->map_size))) ||
		    ((address >= pERAM->ephy_base) && (address < (pERAM->ephy_base + pERAM->emap_size))))
		{
			isexternal = E_TRUE;
		} else {
			isexternal = E_FALSE;
			coreid     = address >> 20;
			isonchip   = e_is_addr_on_chip((void *) address);
			if (isonchip)
			{
				ee_get_coords_from_id(pEpiphany, coreid, &row, &col);
				ismems  = (address <  pEpiphany->core[row][col].mems.phy_base + pEpiphany->core[row][col].mems.map_size);
				isregs  = (address >= pEpiphany->core[row][col].regs.phy_base);
			}
		}
	}

	if (isglobal)
	{
		if (isexternal)
		{
			diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): to external memory.\n"); }
			if ((address >= pERAM->ephy_base) && (address < (pERAM->ephy_base + pERAM->emap_size)))
			{
				diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): converting virtual (0x%08x) ", address); }
				address = address - (pERAM->ephy_base - pERAM->phy_base);
				diag(H_D2) { fprintf(diag_fd, "to physical (0x%08x)...\n", address); }
			}
			diag(H_D2) { fprintf(diag_fd, "ee_read_abs(): converting physical (0x%08x) ", address); }
			address = address - pERAM->phy_base;
			diag(H_D2) { fprintf(diag_fd, "to offset (0x%08x)...\n", address); }
			rcount = ee_mwrite_buf(pERAM, address, buf, burst_size);
			diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): isexternal -> rcount = %d\n", (int) rcount); }
		} else if (isonchip)
		{
			address = address & 0x000fffff;
			if (ismems) {
				rcount = ee_write_buf(pEpiphany, row, col, address, buf, burst_size);
				diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): isonchip/ismems -> rcount = %d\n", (int) rcount); }
			} else if (isregs) {
				for (rcount=0, i=0; i<burst_size; i+=sizeof(unsigned)) {
					rcount += ee_write_reg(pEpiphany, row, col, address, *((unsigned *)(buf+i)));
				}
				diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): isonchip/isregs -> rcount = %d\n", (int) rcount); }
			} else {
				rcount = 0;
				diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): is a reserved on-chip address -> rcount = %d\n", (int) rcount); }
			}
		} else {
			rcount = 0;
			diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): is not a legal address -> rcount = %d\n", (int) rcount); }
		}
	} else {
		rcount = 0;
		diag(H_D1) { fprintf(diag_fd, "ee_write_abs(): is not a global address -> rcount = %d\n", (int) rcount); }
	}

	return rcount;
}


// TODO: replace with platform data
#define EMEM_SIZE (0x02000000)

int esrv_init_platform(platform_definition_t *platform_arg, unsigned verbose_mode)
{
	int res;

	pEpiphany = &Epiphany;
	pERAM     = &ERAM;
	platform  = platform_arg;

	e_set_host_verbosity(verbose_mode);
	e_init(NULL);
	res = e_alloc(pERAM, 0, EMEM_SIZE);
	res = e_open(pEpiphany, 0, 0, e_platform.rows, e_platform.cols);
	// TODO: this assumes a contiguous array of chips.

	return res;
}


int esrv_close_platform()
{
	int res;

	res = e_close(pEpiphany);
	res = e_free(pERAM);
	e_finalize();

	return res;
}



int esrv_write_to(unsigned address, void *buf, size_t burst_size)
{
	// The readMem() function which calls read_from() driver function always calls with a global address.
	// need to check which region is being called and use the appropriate e-host API.

	ssize_t rcount;

	rcount = ee_write_abs(address, buf, burst_size);

	return rcount;
}



int esrv_read_from(unsigned address, void* buf, size_t burst_size)
{
	// The readMem() function which calls read_from() driver function always calls with a global address.
	// need to check which region is being called and use the appropriate e-host API.

	ssize_t rcount;

	rcount = ee_read_abs(address, buf, burst_size);

	return rcount;
}



int esrv_hw_reset()
{
	int sleepTime = 0;

	e_reset_system();

	sleep(sleepTime);

	return 0;
}


int esrv_get_description(char** targetIdp)
{
	*targetIdp = platform->name;

	return 0;
}

#pragma GCC diagnostic pop
