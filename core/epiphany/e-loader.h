
#ifndef __E_LOADER_H__
#define __E_LOADER_H__

#include "e-hal.h"

#ifdef __cplusplus
extern "C"
{
#endif

typedef enum {
	L_D0 = 0,
	L_D1 = 1,
	L_D2 = 2,
	L_D3 = 3,
	L_D4 = 40,
} e_loader_diag_t;

int e_load(char *executable, e_epiphany_t *dev, unsigned row, unsigned col, e_bool_t start);
int e_load_group(char *executable, e_epiphany_t *dev, unsigned row, unsigned col, unsigned rows, unsigned cols, e_bool_t start);

e_loader_diag_t e_set_loader_verbosity(e_loader_diag_t verbose);

#ifdef __cplusplus
}
#endif

#endif // __E_LOADER_H__
