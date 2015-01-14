/*
  File: e-process-SREC.c

  This file is part of the Epiphany Software Development Kit.

  Copyright (C) 2013 Adapteva, Inc.
  See AUTHORS for list of contributors.
  Support e-mail: <support@adapteva.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License (LGPL)
  as published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  and the GNU Lesser General Public License along with this program,
  see the files COPYING and COPYING.LESSER.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <elf.h>
#include <err.h>

#include "e-hal.h"
#include "epiphany-hal-api-local.h"
#include "e-loader.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

typedef unsigned int uint;


#define diag(vN)   if (e_load_verbose >= vN)

#define MAX_NUM_WRITE_PACKETS 256
#define MAX_NUM_READ_PACKETS   64
#define MAX_BUFFER_TO_SERVER_SIZE (MAX_NUM_WRITE_PACKETS * 8)

#define BUFSIZE 1000  // should be sufficient for an SREC record

extern int e_load_verbose;
extern FILE *diag_fd;


e_return_stat_t ee_process_ELF(char *executable, e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col)
{
	FILE       *elfStream;
	Elf32_Ehdr hdr;
	Elf32_Phdr *phdr;
	e_bool_t   islocal, isonchip;
	int        ihdr;
	void       *pto;
	unsigned   globrow, globcol;
	unsigned   CoreID;
	int        status = E_OK;


	islocal   = E_FALSE;
	isonchip  = E_FALSE;

	elfStream = fopen(executable, "rb");

	fread(&hdr, sizeof(hdr), 1, elfStream);
	phdr = alloca(sizeof(*phdr) * hdr.e_phnum);
	fseek(elfStream, hdr.e_phoff, SEEK_SET);
	fread(phdr, sizeof(*phdr), hdr.e_phnum, elfStream);

	for (ihdr=0; ihdr<hdr.e_phnum; ihdr++)
	{
		if (phdr[ihdr].p_vaddr & 0xfff00000)
		{
			// This is a global address. Check if address is on an eCore.
			islocal  = E_FALSE;
			isonchip = e_is_addr_on_chip((void *) phdr[ihdr].p_vaddr);
		} else {
			// This is a local address.
			islocal  = E_TRUE;
			isonchip = E_TRUE;
		}

		diag(L_D3) { fprintf(diag_fd, "ee_process_ELF(): copying the data (%d bytes)", phdr[ihdr].p_filesz); }

		if (islocal)
		{
			// If this is a local address
			diag(L_D3) { fprintf(diag_fd, " to core (%d,%d)\n", row, col); }
			pto = pEpiphany->core[row][col].mems.base + phdr[ihdr].p_vaddr; // TODO: should this be p_paddr instead of p_vaddr?
		}
		else if (isonchip)
		{
			// If global address, check if address is of an eCore.
			CoreID = phdr[ihdr].p_vaddr >> 20;
			ee_get_coords_from_id(pEpiphany, CoreID, &globrow, &globcol);
			diag(L_D3) { fprintf(diag_fd, " to core (%d,%d)\n", globrow, globcol); }
			pto = pEpiphany->core[globrow][globcol].mems.base + (phdr[ihdr].p_vaddr & ~(0xfff00000)); // TODO: should this be p_paddr instead of p_vaddr?
		}
		else
		{
			// If it is not on an eCore, it is on external memory.
			diag(L_D3) { fprintf(diag_fd, " to external memory.\n"); }
			pto = (void *) phdr[ihdr].p_vaddr;
			if ((phdr[ihdr].p_vaddr >= pEMEM->ephy_base) && (phdr[ihdr].p_vaddr < (pEMEM->ephy_base + pEMEM->emap_size)))
			{
				diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): converting virtual (0x%08x) ", (uint) phdr[ihdr].p_vaddr); }
				pto = pto - (pEMEM->ephy_base - pEMEM->phy_base);
				diag(L_D3) { fprintf(diag_fd, "to physical (0x%08x)...\n", (uint) phdr[ihdr].p_vaddr); }
			}
			diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): converting physical (0x%08x) ", (uint) phdr[ihdr].p_vaddr); }
			pto = pto - (uint) pEMEM->phy_base + (uint) pEMEM->base;
			diag(L_D3) { fprintf(diag_fd, "to offset (0x%08x)...\n", (uint) pto); }
		}
		fseek(elfStream, phdr[ihdr].p_offset, SEEK_SET);
		fread(pto, phdr[ihdr].p_filesz, sizeof(char), elfStream);
	}

	fclose(elfStream);

	return status;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
e_return_stat_t ee_process_SREC(char *executable, e_epiphany_t *pEpiphany, e_mem_t *pEMEM, int row, int col)
{
	typedef enum {S0, S3, S7} SrecSel;
	FILE      *srecStream;
	int      i;
	char     buf[BUFSIZE], *pbuf;
	e_bool_t insection;
	e_bool_t islocal, isonchip;
	unsigned CoreID;
	int      status = E_OK;


	insection = E_FALSE;
	islocal   = E_FALSE;
	isonchip  = E_FALSE;

	diag(L_D1) { fprintf(diag_fd, "ee_process_SREC(): loading core (%d,%d).\n", row, col); }

	srecStream = fopen(executable, "r");
	if (srecStream == NULL)
	{
		fprintf(diag_fd, "Error: Can't open SREC file: %s\n", executable);
		return E_ERR;
	}

	rewind(srecStream);
	unsigned lineN = 0;
    char *dummy    = NULL;

	while(!feof(srecStream) && !ferror(srecStream))
	{
        dummy = fgets(buf, BUFSIZE, srecStream);

		if (!feof(srecStream) && !ferror(srecStream))
		{
			diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): %s", buf); }

			// RECORD TYPE
			SrecSel sSel;
			unsigned addrSize;

			if (buf[0] != 'S')
			{
				warnx("Error: Invalid record format in SREC file line ");
				warnx("%d: \"%s\"\n", lineN, buf);
				return E_ERR;
			}

			if (buf[1] == '0')
			{
				sSel = S0;
				addrSize = 4;
				if (insection == E_TRUE)
				{
					warnx("Error: S0 record found inside a section in SREC file line ");
					warnx("%d: \"%s\"\n", lineN, buf);
					status = E_WARN;
					continue; // TODO: bail out with error code
				} else {
					insection = E_TRUE;
				}
			} else if (buf[1] == '3')
			{
				sSel = S3;
				addrSize = 8;
				if (insection == E_FALSE)
				{
					warnx("Error: S3 record found outside of a section in SREC file line ");
					warnx("%d: \"%s\"\n", lineN, buf);
					status = E_WARN;
					continue; // TODO: bail out with error code
				}
			} else if (buf[1] == '7')
			{
				sSel = S7;
				addrSize = 8;
				if (insection == E_FALSE)
				{
					warnx("Error: S7 record found outside of a section in SREC file line ");
					warnx("%d: \"%s\"\n", lineN, buf);
					status = E_WARN;
					continue; // TODO: bail out with error code
				} else {
					insection = E_FALSE;
				}
			} else
			{
				warnx("Error: Invalid record types (valid types are S0, S3 and S7) in SREC file line ");
				warnx("%d: \"%s\"\n", lineN, buf);
				status = E_WARN;
				continue; // TODO: bail out with error code
			}


			// BYTES COUNT
			char byteCountStr[5];
			byteCountStr[0] = '0';
			byteCountStr[1] = 'x';
			byteCountStr[2] = buf[2];
			byteCountStr[3] = buf[3];
			byteCountStr[4] = '\0';
			unsigned byteCount = strtol(byteCountStr, NULL, 16);
			if (byteCount > 0)
			{
				byteCount = byteCount - (addrSize/2) /* addr */ - 1 /* checksum */;
				diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): record length = %d\n", byteCount); }
			} else {
				warnx("Error: Wrong record format in SREC file line ");
				warnx("%d: \"%s\"\n", lineN, buf);
				status = E_WARN;
				continue;
			}


			// ADDRESS
			unsigned long addrHex;

			char addrBuf[9];
			strncpy(addrBuf, buf+4, addrSize);
			addrBuf[addrSize] = '\0';
			addrHex = strtoll(addrBuf, NULL, 16);
			if (addrHex & 0xfff00000)
			{
				// This is a global address. Check if address is on an eCore.
				islocal  = E_FALSE;
				isonchip = e_is_addr_on_chip((void *) addrHex);
			} else {
				// This is a local address.
				islocal  = E_TRUE;
				isonchip = E_TRUE;
			}
			diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): address: 0x%08x\n", (uint) addrHex); }


			// DATA
			if (sSel == S0)
			{
				// Start of Core section
				char dataBuf[5];
//				int srecnum;

				pbuf = buf + 4;
				strncpy(dataBuf, pbuf, 4);
				dataBuf[4] = '\0';

				diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): %x\n", (uint) dataBuf); }

/*				CoreID = strtol(dataBuf, NULL, 16);
				diag(L_D2) { fprintf(diag_fd, "ee_process_SREC(): found coreID 0x%03x in the S0 record.\n", (uint) CoreID); }

				if (CoreID == 0x000)
				{
					corenum = loopnum;
					CoreID = ee_get_id_from_num(pEpiphany, loopnum);
				} else {
					srecnum = ee_get_num_from_id(pEpiphany, CoreID);
					corenum = srecnum;
				}
*/
			}


			if (sSel == S3)
			{
				// Core data record
				unsigned char Data2Send[MAX_BUFFER_TO_SERVER_SIZE];
				unsigned int globrow, globcol;

				assert(byteCount <= MAX_BUFFER_TO_SERVER_SIZE);
				diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): copying the data (%d bytes)", byteCount); }

				// convert text to bytes
				pbuf = buf + 4 + addrSize;
				for (i=0; i<byteCount; i++)
				{
					char dataBuf[3];
					dataBuf[0] = *(pbuf++);
					dataBuf[1] = *(pbuf++);
					dataBuf[2] = '\0';

					unsigned long dataHex = strtol(dataBuf, NULL, 16);
					Data2Send[i] = dataHex;

					diag(L_D4) { fprintf(diag_fd, "ee_process_SREC():  %s", dataBuf); }
				}

				if (islocal)
				{
					// If this is a local address
					diag(L_D3) { fprintf(diag_fd, " to core (%d,%d)\n", row, col); }
					ee_write_buf(pEpiphany, row, col, addrHex, Data2Send, byteCount);
				}
				else if (isonchip)
				{
					// If global address, check if address is of an eCore.
					CoreID = addrHex >> 20;
					ee_get_coords_from_id(pEpiphany, CoreID, &globrow, &globcol);
					diag(L_D3) { fprintf(diag_fd, " to core (%d,%d)\n", globrow, globcol); }
					ee_write_buf(pEpiphany, globrow, globcol, addrHex & ~(0xfff00000), Data2Send, byteCount);
				}
				else
				{
					// If it is not on an eCore, it is on external memory.
					diag(L_D3) { fprintf(diag_fd, " to external memory.\n"); }
					if ((addrHex >= pEMEM->ephy_base) && (addrHex < (pEMEM->ephy_base + pEMEM->emap_size)))
					{
						diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): converting virtual (0x%08x) ", (uint) addrHex); }
						addrHex = addrHex - (pEMEM->ephy_base - pEMEM->phy_base);
						diag(L_D3) { fprintf(diag_fd, "to physical (0x%08x)...\n", (uint) addrHex); }
					}
					diag(L_D3) { fprintf(diag_fd, "ee_process_SREC(): converting physical (0x%08x) ", (uint) addrHex); }
					addrHex = addrHex - pEMEM->phy_base;
					diag(L_D3) { fprintf(diag_fd, "to offset (0x%08x)...\n", (uint) addrHex); }
					ee_mwrite_buf(pEMEM, addrHex, Data2Send, byteCount);
				}
			}


			if (sSel == S7)
			{
				// End of Core section
				char startAddrofProg[9];

				pbuf = buf + 4;
				strncpy(startAddrofProg, pbuf, addrSize);
				startAddrofProg[addrSize] = '\0';
				unsigned long startOfProrgram = strtol(startAddrofProg, NULL, 16);
				if (startOfProrgram != 0)
				{
					warnx("Warning: non zero _start address. The start of program is detected in the address ");
					warnx("%x\n", (unsigned int) startOfProrgram);
					status = E_WARN;
				}
			}
			lineN++;
		}
	}

	fclose(srecStream);

	return status;
}
#pragma GCC diagnostic pop  /* unused-but-set-variable */
#pragma GCC diagnostic pop  /* sign-compare */

