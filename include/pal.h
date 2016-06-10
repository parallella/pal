#pragma once

#include "pal_base.h"
#include "pal_math.h"
#include "pal_dsp.h"
#include "pal_image.h"
#include "pal_fft.h"

#ifdef PAL_SOURCE
# include "common.h"
#endif

#ifdef __epiphany__
/* Static library. Force a dependency so __pal_init/fini always will be
 * included in built binary. Not pretty but works. */
extern void __pal_init();
void (*__pal_init_p) (void) __attribute__((weak)) = __pal_init;
#endif
