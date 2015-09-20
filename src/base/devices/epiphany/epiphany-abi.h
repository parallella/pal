#pragma once
#include <stdint.h>

/* Structs needed by e-lib */

typedef enum {
    E_NULL         = 0,
    E_EPI_PLATFORM = 1,
    E_EPI_CHIP     = 2,
    E_EPI_GROUP    = 3,
    E_EPI_CORE     = 4,
    E_EXT_MEM      = 5,
    E_MAPPING      = 6,
    E_SHARED_MEM   = 7
} e_objtype_t;

typedef enum {
    E_E16G301 = 0,
    E_E64G401 = 1,
} e_chiptype_t;

typedef struct {
    uint32_t objtype;           // 0x28
    uint32_t chiptype;          // 0x2c
    uint32_t group_id;          // 0x30
    uint32_t group_row;         // 0x34
    uint32_t group_col;         // 0x38
    uint32_t group_rows;        // 0x3c
    uint32_t group_cols;        // 0x40
    uint32_t core_row;          // 0x44
    uint32_t core_col;          // 0x48
    uint32_t alignment_padding; // 0x4c
} __attribute__((packed)) e_group_config_t;

typedef struct {
    uint32_t objtype;           // 0x50
    uint32_t base;              // 0x54
} __attribute__((packed)) e_emem_config_t;

#ifdef __epiphany__
extern const e_group_config_t e_group_config;
extern const e_emem_config_t  e_emem_config;
#endif

