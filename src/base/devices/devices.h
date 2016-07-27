#pragma once
#if HOST_IS_POSIX
# if ENABLE_DEV_EPIPHANY
#  include "posix/epiphany/dev_epiphany.h"
# endif
# if ENABLE_DEV_GRID
/* TODO */
# endif
# if ENABLE_DEV_SMP
/* TODO */
# endif
# if ENABLE_DEV_FPGA
/* TODO */
# endif
# if ENABLE_DEV_GPU
/* TODO */
# endif
# if ENABLE_DEV_DEMO
/* TODO */
# endif
#elif HOST_IS_EPIPHANY
# if ENABLE_DEV_EPIPHANY
#  include "epiphany/epiphany/dev_epiphany.h"
# endif
# if ENABLE_DEV_GRID
/* TODO */
# endif
# if ENABLE_DEV_SMP
/* TODO */
# endif
# if ENABLE_DEV_FPGA
/* TODO */
# endif
# if ENABLE_DEV_GPU
/* TODO */
# endif
# if ENABLE_DEV_DEMO
/* TODO */
# endif
#endif /* HOST_IS_EPIPHANY */
