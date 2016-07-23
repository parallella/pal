#pragma once

#include <errno.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "../math/tinymt/tinymt32.h"

// TEMPORARY HACK!!
struct p_dev
{
    int property[32];
};

struct p_team
{
    p_dev_t *dev;   // pointer to the associated device structure
    int size;       // number of member nodes
    p_team_t *team; // list of addresses, one per node (int)
    p_team_t *stat; // list of status "regs". idle/working
};

// Optimize later, focus on function...
struct p_mem
{
    p_team_t *team; // pointer to the associated team structure
    int mutex;      // optional mutex to grab before reading/writing 'mem'
    int takeit;     // indicates that new data is ready (wraparound impl)
    int gotit;      // indicates that data was read (wraparound impl)
    int pilot;      // temp var used for flushing read/write path to 'mem'
    size_t size;    // size of memory buffer
    void *mem;      // pointer to allocated memory
};

struct p_program
{
    p_dev_t *dev; // pointer to the associated device structure
    char *name;   // executable file name
};

struct p_symbol
{
};

struct p_event
{
};

struct p_atom_u32
{
    uint32_t mutex; // resource mutex
    uint32_t var;   // atomic variable
};

/*
 ***********************************************************************
 * Global system variables (will be replaced with O/S real time
 ***********************************************************************
 */

struct dev_ops;
struct dev;
struct rank_range;
struct team;
struct prog;
struct pal_global;

struct dev_ops {
    /* These must be defined */
    p_dev_t (*init) (struct dev *, int);
    void (*fini) (struct dev *);

    int (*query) (struct dev *, int);
    struct team *(*open) (struct team *);
    int (*close) (struct dev *, struct team *);
    int (*load) (struct team *, int, int, struct prog *, const char *, int,
                 const p_arg_t *);
    int (*start) (struct team *, int, int);
    int (*wait) (struct dev *, struct team *);
    int (*kill) (struct team *, int, int, int);
    void * (*map_member) (struct team *, int, unsigned long, unsigned long);
    p_mem_t (*map) (struct dev *, unsigned long, unsigned long);
    int (*unmap) (struct team *, p_mem_t *);

    /* Optional */
    int (*early_init) (struct dev *);
    void (*late_fini) (struct dev *);

    void * (*_map_raw) (struct dev *, unsigned long, unsigned long);
};

struct dev {
    struct dev_ops *dev_ops;
};

#if 0
struct rank_range {
    int first;
    int n;
};
#endif

struct team {
    struct team *next;
    struct dev *dev; // Support only one device per team (at leat for now)
#if 1
    int start;
    int count;
#else
    struct rank_range *ranges;
    size_t ranges_size;
#endif
};

struct prog {
    struct prog *next;
    struct dev *dev;
    char *name; // Must be '\0' terminated
    char *path; // Must be '\0' terminated
    char *buf;
    size_t buf_size;
};

struct pal_global {
    struct dev  *devs[P_DEV_LAST+1];
    struct team *teams_head;
    struct team *teams_tail;
    struct prog *progs_head;
    struct prog *progs_tail;
#if 0
    struct symbol *symbols_head;
    struct symbol *symbols_tail;
    struct event *events_head;
    struct event *events_tail;
    struct mem *mems_head;
    struct mem *mems_tail;
    struct atom *atoms_head;
    struct atom *atoms_tail;
    struct mutex *mutexes_head;
    struct mutex *mutexes_tail;
#endif
    tinymt32_t random; // PRNG state

    uint32_t rank;
};

struct mem_ops {
    ssize_t (*write) (p_mem_t *, const void *, off_t, size_t, int);
    ssize_t (*read) (p_mem_t *, void *, off_t, size_t, int);
};

extern struct pal_global __pal_global;


/*
 ***********************************************************************
 * Error handling
 ***********************************************************************
 */

/* Equivalent to generic Linux Kernel implementation of passing error codes
 * through pointers. This restricts us from returning pointers in the
 * upper-most 4096 byte address range, which should be fine considering it is
 * reserved for stack on most systems. Another option would be to use the
 * lowest bit to indicate error conditions.
 */

#define P_REF_ERR_MAX 4095

/* Convert an error code to a reference */
static inline p_ref_t p_ref_err(const int err)
{
    return (p_ref_t) ((intptr_t) -err);
}

static inline p_mem_t p_mem_err(const int err)
{
    p_mem_t mem = { 0 };
    mem.ref = p_ref_err(err);
    return mem;
}


static inline bool p_ref_is_err(const p_ref_t ref)
{
    return (((uintptr_t) ref) >= ((uintptr_t) -P_REF_ERR_MAX));
}

static inline int p_ref_get_err(const p_ref_t ref)
{
    if (p_ref_is_err(ref))
        return (int) ((intptr_t) ref);
    else
        return 0;
}

/*
 ***********************************************************************
 * CPU YIELDING
 ***********************************************************************
 */

/*
 * Prevents excessive wastage of processing units if lock is contended
 * when possible
 */

#if defined(__i386) || defined(__x86_64__)
#define p_cpu_relax() __asm__ volatile("pause\n": : :"memory")
#elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) \
	|| defined(__powerpc64__) || defined(__ppc64__) || defined(__PPC64__)
#define p_cpu_relax() __asm__ volatile("or 27,27,27\n": : :"memory")
#elif defined(__ARM_ARCH_6T2__) || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) \
	|| defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) \
	|| defined(__ARM_ARCH_7S__) || defined(__aarch64__)
#define p_cpu_relax() __asm__ volatile("yield\n": : :"memory")
#elif defined(__FreeBSD__) || defined(__linux__) || defined(__NetBSD__) \
	|| defined(__OpenBSD__) || defined(__MACH__)
/* Many OSes support it, but some require odd macros to get the declaration */
int pthread_yield(void);
#define p_cpu_relax() pthread_yield ()
#else
#define p_cpu_relax() ((int) 0)
#endif
