

#ifndef __E_HAL_DATA_H__
#define __E_HAL_DATA_H__

#include <semaphore.h>

#include "epiphany-hal-data-local.h"

#ifdef __cplusplus
extern "C"
{
typedef enum {
	E_FALSE = false,
	E_TRUE	= true,
} e_bool_t;
#else
typedef enum {
	E_FALSE = 0,
	E_TRUE	= 1,
} e_bool_t;
#endif


typedef enum {
	H_D0 = 0,
	H_D1 = 1,
	H_D2 = 2,
	H_D3 = 3,
	H_D4 = 4,
} e_hal_diag_t;


typedef enum {
	E_OK   =  0,
	E_ERR  = -1,
	E_WARN = -2,
} e_return_stat_t;


// eCore General Purpose Registers
typedef enum {
	E_REG_R0	= 0xf0000,
	E_REG_R1	= 0xf0004,
	E_REG_R2	= 0xf0008,
	E_REG_R3	= 0xf000c,
	E_REG_R4	= 0xf0010,
	E_REG_R5	= 0xf0014,
	E_REG_R6	= 0xf0018,
	E_REG_R7	= 0xf001c,
	E_REG_R8	= 0xf0020,
	E_REG_R9	= 0xf0024,
	E_REG_R10	= 0xf0028,
	E_REG_R11	= 0xf002c,
	E_REG_R12	= 0xf0030,
	E_REG_R13	= 0xf0034,
	E_REG_R14	= 0xf0038,
	E_REG_R15	= 0xf003c,
	E_REG_R16	= 0xf0040,
	E_REG_R17	= 0xf0044,
	E_REG_R18	= 0xf0048,
	E_REG_R19	= 0xf004c,
	E_REG_R20	= 0xf0050,
	E_REG_R21	= 0xf0054,
	E_REG_R22	= 0xf0058,
	E_REG_R23	= 0xf005c,
	E_REG_R24	= 0xf0060,
	E_REG_R25	= 0xf0064,
	E_REG_R26	= 0xf0068,
	E_REG_R27	= 0xf006c,
	E_REG_R28	= 0xf0070,
	E_REG_R29	= 0xf0074,
	E_REG_R30	= 0xf0078,
	E_REG_R31	= 0xf007c,
	E_REG_R32	= 0xf0080,
	E_REG_R33	= 0xf0084,
	E_REG_R34	= 0xf0088,
	E_REG_R35	= 0xf008c,
	E_REG_R36	= 0xf0090,
	E_REG_R37	= 0xf0094,
	E_REG_R38	= 0xf0098,
	E_REG_R39	= 0xf009c,
	E_REG_R40	= 0xf00a0,
	E_REG_R41	= 0xf00a4,
	E_REG_R42	= 0xf00a8,
	E_REG_R43	= 0xf00ac,
	E_REG_R44	= 0xf00b0,
	E_REG_R45	= 0xf00b4,
	E_REG_R46	= 0xf00b8,
	E_REG_R47	= 0xf00bc,
	E_REG_R48	= 0xf00c0,
	E_REG_R49	= 0xf00c4,
	E_REG_R50	= 0xf00c8,
	E_REG_R51	= 0xf00cc,
	E_REG_R52	= 0xf00d0,
	E_REG_R53	= 0xf00d4,
	E_REG_R54	= 0xf00d8,
	E_REG_R55	= 0xf00dc,
	E_REG_R56	= 0xf00e0,
	E_REG_R57	= 0xf00e4,
	E_REG_R58	= 0xf00e8,
	E_REG_R59	= 0xf00ec,
	E_REG_R60	= 0xf00f0,
	E_REG_R61	= 0xf00f4,
	E_REG_R62	= 0xf00f8,
	E_REG_R63	= 0xf00fc,
} e_gp_reg_id_t;

// eCore Special Registers
typedef enum {
	// Control registers
	E_REG_CONFIG	   = 0xf0400,
	E_REG_STATUS	   = 0xf0404,
	E_REG_PC	   = 0xf0408,
	E_REG_DEBUGSTATUS  = 0xf040c,
	E_REG_LC	   = 0xf0414,
	E_REG_LS	   = 0xf0418,
	E_REG_LE	   = 0xf041c,
	E_REG_IRET	   = 0xf0420,
	E_REG_IMASK	   = 0xf0424,
	E_REG_ILAT	   = 0xf0428,
	E_REG_ILATST	   = 0xf042C,
	E_REG_ILATCL	   = 0xf0430,
	E_REG_IPEND	   = 0xf0434,
	E_REG_CTIMER0	   = 0xf0438,
	E_REG_CTIMER1	   = 0xf043C,
	E_REG_FSTATUS	   = 0xf0440,
	E_REG_DEBUGCMD	   = 0xf0448,
	E_REG_DMA0CONFIG   = 0xf0500,
	E_REG_DMA0STRIDE   = 0xf0504,
	E_REG_DMA0COUNT	   = 0xf0508,
	E_REG_DMA0SRCADDR  = 0xf050C,
	E_REG_DMA0DSTADDR  = 0xf0510,
	E_REG_DMA0AUTODMA0 = 0xf0514,
	E_REG_DMA0AUTODMA1 = 0xf0518,
	E_REG_DMA0STATUS   = 0xf051C,
	E_REG_DMA1CONFIG   = 0xf0520,
	E_REG_DMA1STRIDE   = 0xf0524,
	E_REG_DMA1COUNT	   = 0xf0528,
	E_REG_DMA1SRCADDR  = 0xf052C,
	E_REG_DMA1DSTADDR  = 0xf0530,
	E_REG_DMA1AUTODMA0 = 0xf0534,
	E_REG_DMA1AUTODMA1 = 0xf0538,
	E_REG_DMA1STATUS   = 0xf053C,
	E_REG_MEMSTATUS	   = 0xf0604,
	E_REG_MEMPROTECT   = 0xf0608,
	E_REG_MESHCONFIG   = 0xf0700,
	E_REG_COREID	   = 0xf0704,
	E_REG_MULTICAST	   = 0xf0708,
	E_REG_RESETCORE	   = 0xf070c,
	E_REG_CMESHROUTE   = 0xf0710,
	E_REG_XMESHROUTE   = 0xf0714,
	E_REG_RMESHROUTE   = 0xf0718
} e_core_reg_id_t;


// Chip registers
typedef enum {
	E_REG_LINKCLK	  = 0xf0300,
	E_REG_LINKTX	  = 0xf0304,
	E_REG_LINKRX	  = 0xf0308,
	E_REG_GPIOCFG	  = 0xf030c,
	E_REG_IOFLAG	  = 0xf0318,
	E_REG_CHIPSYNC	  = 0xf031c,
	E_REG_CHIPHALT	  = 0xf0320,
	E_REG_CHIPRESET	  = 0xf0324,
	E_REG_LINKDEBUG	  = 0xf0328,	
} e_chip_regs_t;


// Epiphany system registers
typedef enum {
	E_REG_SYSRESET	  = 0xf0340,
	E_REG_SYSCFGTX	  = 0xf0344,
	E_REG_SYSCFGRX	  = 0xf0348,
	E_REG_SYSCFGCLK	  = 0xf034c,
	E_REG_SYSCOREID   = 0xf0350,
	E_REG_SYSVERSION  = 0xf0354,
	E_REG_SYSDATAIN   = 0xf0358,
	E_REG_SYSDATAOUT  = 0xf035c,
	E_REG_MBOXSTATUS  = 0xf0360,
	E_REG_MBOX0       = 0xf0364,
	E_REG_MBOX1       = 0xf0368,
	E_REG_SYSMONCFG   = 0xf036c,
        E_REG_SYSRXMON0   = 0xf0370,
        E_REG_SYSRXMON1   = 0xf0374,
        E_REG_SYSRXMON2   = 0xf0378,
        E_REG_SYSTXMON0   = 0xf037c,
        E_REG_SYSTXMON1   = 0xf0380,
        E_REG_SYSTXMON2   = 0xf0384,
	E_REG_MMU	  = 0xf8000,
} e_sys_reg_id_t;

// Core group data structures
typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	e_chiptype_t	 type;		  // Epiphany chip part number
	unsigned int	 num_cores;	  // number of cores group
	unsigned int	 base_coreid; // group base core ID
	unsigned int	 row;		  // group absolute row number
	unsigned int	 col;		  // group absolute col number
	unsigned int	 rows;		  // number of rows group
	unsigned int	 cols;		  // number of cols group
	e_core_t	   **core;		  // e-cores data structures array
	int				 memfd;		  // for mmap
} e_epiphany_t;


typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	off_t			 phy_base;	  // physical global base address of external memory buffer as seen by host side
	off_t			 page_base;	  // physical base address of memory page
	off_t			 page_offset; // offset of memory region base to memory page base
	size_t			 map_size;	  // size of eDRAM allocated buffer for host side
	off_t			 ephy_base;	  // physical global base address of external memory buffer as seen by device side
	size_t			 emap_size;	  // size of eDRAM allocated buffer for device side
	void			*mapped_base; // for mmap
	void			*base;		  // application (virtual) space base address of external memory buffer
	int				 memfd;		  // for mmap
} e_mem_t;

#define ALIGN(x)	__attribute__ ((aligned (x)))

#define MAX_SHM_REGIONS				   64

/*
** Type definitions
*/
#pragma pack(push, 1)

/** Shared memory segment */
typedef struct ALIGN(8) e_shmseg {
	void     *addr;         /* Virtual address */
	char      name[256];    /* Region name */
	size_t    size;         /* Region size in bytes */
	void     *paddr;        /* Physical Address accessible from Epiphany cores */
	off_t     offset;       /* Offset from shm base address */
} e_shmseg_t;

typedef struct ALIGN(8) e_shmseg_pvt	{
	e_shmseg_t      shm_seg;  /* The shared memory segment */
	unsigned        refcnt;   /* host app reference count */
	unsigned        valid;    /* 1 if the region is in use, 0 otherwise */
} e_shmseg_pvt_t;

typedef struct ALIGN(8) e_shmtable {
	unsigned int     magic;
	unsigned int     initialized;
	e_shmseg_pvt_t   regions[MAX_SHM_REGIONS];
	unsigned long    paddr_epi; /* Physical address of the shm region as seen by epiphany */
	unsigned long    paddr_cpu;	/* Physical address of the shm region as seen by the host cpu */
} e_shmtable_t;

#pragma pack(pop)

#ifdef __cplusplus
}
#endif

#endif // __E_HAL_DATA_H__
