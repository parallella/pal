AUTOMAKE_OPTIONS = foreign
SUBDIRS = examples
EXTRA_DIST = epiphany

# examples depends on this
BUILT_SOURCES = libpal.la

lib_LTLIBRARIES = libpal.la
libpal_la_SOURCES = \
    p_append.c \
    p_atomic_add.c \
    p_atomic_and.c \
    p_atomic_compswap.c \
    p_atomic_or.c \
    p_atomic_sub.c \
    p_atomic_swap.c \
    p_atomic_xor.c \
    p_barrier.c \
    p_close.c \
    p_fence.c \
    p_finalize.c \
    p_free.c \
    p_gather.c \
    p_getaddr.c \
    p_getsymbol.c \
    p_init.c \
    p_load.c \
    p_malloc.c \
    p_memcpy.c \
    p_mutex_destroy.c \
    p_mutex_init.c \
    p_mutex_lock.c \
    p_mutex_trylock.c \
    p_mutex_unlock.c \
    p_open.c \
    p_query.c \
    p_read.c \
    p_remove.c \
    p_rmalloc.c \
    p_run.c \
    p_wait.c \
    p_write.c \
    pal_base_private.h

# Disable temporarily until API is determined
#    p_scatter.c \
#    p_broadcast.c \
#

