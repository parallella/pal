
#ifndef __E_HAL_API_LOC_H__
#define __E_HAL_API_LOC_H__

#include <sys/types.h>

#ifdef __cplusplus
extern "C"
{
#endif

#define EPIPHANY_DEV "/dev/epiphany"

/////////////////////////////////
// Device communication functions
//
int      ee_read_word(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr);
ssize_t  ee_write_word(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, int data);
ssize_t  ee_read_buf(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr, void *buf, size_t size);
ssize_t  ee_write_buf(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, const void *buf, size_t size);
int      ee_read_reg(e_epiphany_t *dev, unsigned row, unsigned col, const off_t from_addr);
ssize_t  ee_write_reg(e_epiphany_t *dev, unsigned row, unsigned col, off_t to_addr, int data);
int      ee_read_esys(off_t from_addr);
ssize_t  ee_write_esys(off_t to_addr, int data);
//
// For legacy code support
ssize_t  ee_read_abs(unsigned address, void *buf, size_t size);
ssize_t  ee_write_abs(unsigned address, void *buf, size_t size);
//
ssize_t  ee_mread(e_mem_t *mbuf, const off_t from_addr, void *buf, size_t size);
ssize_t  ee_mwrite(e_mem_t *mbuf, off_t to_addr, const void *buf, size_t size);
int      ee_mread_word(e_mem_t *mbuf, const off_t from_addr);
ssize_t  ee_mwrite_word(e_mem_t *mbuf, off_t to_addr, int data);
ssize_t  ee_mread_buf(e_mem_t *mbuf, const off_t from_addr, void *buf, size_t size);
ssize_t  ee_mwrite_buf(e_mem_t *mbuf, off_t to_addr, const void *buf, size_t size);


/////////////////////////
// Core control functions
int      ee_reset_core(e_epiphany_t *dev, unsigned row, unsigned col);


////////////////////
// Utility functions
unsigned ee_get_num_from_id(e_epiphany_t *dev, unsigned coreid);
unsigned ee_get_id_from_coords(e_epiphany_t *dev, unsigned row, unsigned col);
unsigned ee_get_id_from_num(e_epiphany_t *dev, unsigned corenum);
void     ee_get_coords_from_id(e_epiphany_t *dev, unsigned coreid, unsigned *row, unsigned *col);
int      ee_set_platform_params(e_platform_t *platform);
int      ee_set_chip_params(e_chip_t *dev);
int      ee_parse_hdf(e_platform_t *dev, char *hdf);
int      ee_parse_simple_hdf(e_platform_t *dev, char *hdf);
int      ee_parse_xml_hdf(e_platform_t *dev, char *hdf);
void     ee_trim_str(char *a);
unsigned long ee_rndu_page(unsigned long size);
unsigned long ee_rndl_page(unsigned long size);

#ifdef __cplusplus
}
#endif

#endif // __E_HAL_API_LOC_H__

