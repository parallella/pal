
#ifndef __E_HAL_DATA_LOC_H__
#define __E_HAL_DATA_LOC_H__


#ifdef __cplusplus
extern "C"
{
#else
#endif


typedef enum {
	E_SYNC	   = 0,
	E_USER_INT = 9,
} e_signal_t;


typedef enum {
	E_RD   = 1,
	E_WR   = 2,
	E_RDWR = 3,
} e_memtype_t;


typedef enum {
	E_NULL		   = 0,
	E_EPI_PLATFORM = 1,
	E_EPI_CHIP	   = 2,
	E_EPI_GROUP	   = 3,
	E_EPI_CORE	   = 4,
	E_EXT_MEM	   = 5,
	E_MAPPING	   = 6,
	E_SHARED_MEM   = 7
} e_objtype_t;


typedef enum {
	E_E16G301 = 0,
	E_E64G401 = 1,
} e_chiptype_t;


typedef enum {
	E_GENERIC		 = 0,
	E_EMEK301		 = 1,
	E_EMEK401		 = 2,
	E_ZEDBOARD1601	 = 3,
	E_ZEDBOARD6401	 = 4,
	E_PARALLELLA1601 = 5,
	E_PARALLELLA6401 = 6,
} e_platformtype_t;


typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	off_t			 phy_base;	  // physical global base address of memory region
	off_t			 page_base;	  // physical base address of memory page
	off_t			 page_offset; // offset of memory region base to memory page base
	size_t			 map_size;	  // size of mapped region
	void			*mapped_base; // for mmap
	void			*base;		  // application space base address of memory region
} e_mmap_t;


typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	unsigned int	 id;		  // core ID
	unsigned int	 row;		  // core absolute row number
	unsigned int	 col;		  // core absolute col number
	e_mmap_t		 mems;		  // core's SRAM data structure
	e_mmap_t		 regs;		  // core's e-regs data structure
} e_core_t;


// Platform data structures
typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	e_chiptype_t	 type;		  // Epiphany chip part number
	char			 version[32]; // version number of Epiphany chip
	unsigned int	 arch;		  // architecture generation
	unsigned int	 base_coreid; // chip base core ID
	unsigned int	 row;		  // chip absolute row number
	unsigned int	 col;		  // chip absolute col number
	unsigned int	 rows;		  // number of rows in chip
	unsigned int	 cols;		  // number of cols in chip
	unsigned int	 num_cores;	  // number of cores in chip
	unsigned int	 sram_base;	  // base offset of core SRAM
	unsigned int	 sram_size;	  // size of core SRAM
	unsigned int	 regs_base;	  // base offset of core registers
	unsigned int	 regs_size;	  // size of core registers segment
	off_t			 ioregs_n;	  // base address of north IO register
	off_t			 ioregs_e;	  // base address of east IO register
	off_t			 ioregs_s;	  // base address of south IO register
	off_t			 ioregs_w;	  // base address of west IO register
} e_chip_t;

typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	off_t			 phy_base;	  // physical global base address of external memory segment as seen by host
	off_t			 ephy_base;	  // physical global base address of external memory segment as seen by devic
	size_t			 size;		  // size of eDRAM allocated buffer for host side
	e_memtype_t		 type;		  // type of memory RD/WR/RW
} e_memseg_t;

typedef struct {
	e_objtype_t		 objtype;	  // object type identifier
	e_platformtype_t type;		  // platform part number
	char			 version[32]; // version number of platform
	unsigned int	 hal_ver;	  // version number of the E-HAL
	int				 initialized; // platform initialized?

	unsigned int	 regs_base;	  // base address of platform registers

	int				 num_chips;	  // number of Epiphany chips in platform
	e_chip_t		*chip;		  // array of Epiphany chip objects
	unsigned int	 row;		  // platform absolute minimum row number
	unsigned int	 col;		  // platform absolute minimum col number
	unsigned int	 rows;		  // number of rows in platform
	unsigned int	 cols;		  // number of cols in platform

	int				 num_emems;	  // number of external memory segments in platform
	e_memseg_t		*emem;		  // array of external memory segments
} e_platform_t;

// Definitions for device workgroup communication object
typedef unsigned int e_coreid_t;

#define SIZEOF_IVT (0x28)

typedef struct {
	e_objtype_t	 objtype;			// 0x28
	e_chiptype_t chiptype;			// 0x2c
	e_coreid_t	 group_id;			// 0x30
	unsigned	 group_row;			// 0x34
	unsigned	 group_col;			// 0x38
	unsigned	 group_rows;		// 0x3c
	unsigned	 group_cols;		// 0x40
	unsigned	 core_row;			// 0x44
	unsigned	 core_col;			// 0x48
	unsigned	 alignment_padding; // 0x4c
} e_group_config_t;

typedef struct {
	e_objtype_t objtype;			// 0x50
	unsigned	base;				// 0x54
} e_emem_config_t;


#ifdef __cplusplus
}
#endif

#endif // __E_HAL_DATA_LOC_H__
