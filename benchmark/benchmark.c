
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <errno.h>

#include "config.h"
#include "benchmark.h"

/* The ratio of the largest known output to input
 * e.g.: p_conv_f32 -> nr+nh-1 -> ratio 2
 */
static const size_t max_output = 3;

/* ifdef posix
 * this prototype is posix specific
 */

#if defined(HAVE_TIME_H)
#include <time.h>
#endif

typedef uint64_t platform_clock_t;

#if defined(HAVE_CLOCK_GETTIME)
static platform_clock_t platform_clock(void)
{
    struct timespec ts;
    uint64_t nanosec;

    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    nanosec = (uint64_t) ts.tv_sec * 1000000000UL + (uint64_t) ts.tv_nsec;

    return nanosec;
}
#endif

static void platform_print_duration(platform_clock_t start,
                                    platform_clock_t end)
{
    printf("%" PRIu64, end - start);
}

/* end of platform specific section */

struct item_data
{
    platform_clock_t start;
};

static void item_preface(struct item_data *, const struct p_bench_item *);
static void item_done(struct item_data *, const struct p_bench_specification *,
                      const char *);
static void setup_memory(struct p_bench_raw_memory *, char **raw, size_t);

static char dummy_memarea[1024 * 1024 * 32];
// int p_bench_dummy_func(char *, size_t);

int main(void)
{
    static const size_t default_initial_size = 655360;

    struct p_bench_specification spec;
    char *raw_mem = NULL;
    spec.current_size = default_initial_size;

    setup_memory(&spec.mem, &raw_mem, spec.current_size);
    printf(";name, size, duration (ns)\n");
    for (const struct p_bench_item *item = benchmark_items; item->name != NULL;
         ++item) {
        struct item_data data;

        item_preface(&data, item);
        item->benchmark(&spec);
        item_done(&data, &spec, item->name);
    }
    return EXIT_SUCCESS;
}

static void setup_output_pointers(struct p_bench_raw_memory *mem, void *p)
{
    /* Assume largest type is 64 bits */

    /* TODO: All pointers point to same memory region so output will be bogus */
    mem->o1.p_u64 = p;
    mem->o2.p_u64 = p;
    mem->o3.p_u64 = p;
    mem->o4.p_u64 = p;
}

static void setup_prandom_chars(char *p, size_t size, unsigned r,
                                bool skip_zero)
{
    /* It is probably not necessary, but this way the same prandom values
     * are used everytime, everywhere
     */

    while (size > 0) {
        r = 7559 * r + 5;
        /* not the best prng method in the universe, but good enough */

        if (skip_zero && ((char)r) == 0) {
            continue;
        }
        *p = (char)r;
        ++p;
        --size;
    }
}

static void setup_input_pointers(struct p_bench_raw_memory *mem, char *p,
                                 size_t size)
{
    unsigned seed = 0;

    /* Assume uint64_t is largest type */

    setup_prandom_chars(p, size * sizeof(uint64_t), seed, false);
    mem->i1_w.p_void = p;
    p += size * sizeof(uint64_t);

    setup_prandom_chars(p, size * sizeof(uint64_t), seed, false);
    mem->i2_w.p_void = p;
    p += size * sizeof(uint64_t);

    setup_prandom_chars(p, size * sizeof(uint64_t), seed, false);
    mem->i3_w.p_void = p;
    p += size * sizeof(uint64_t);

    setup_prandom_chars(p, size * sizeof(uint64_t), seed, false);
    mem->i4_w.p_void = p;
    p += size * sizeof(uint64_t);
}

static void setup_memory(struct p_bench_raw_memory *mem, char **raw,
                         size_t size)
{
    assert(mem != NULL);
    assert(size > 0);
    assert(raw != NULL);

    size_t raw_output_size = sizeof(uintmax_t) * size * max_output;
    size_t raw_size =
        raw_output_size +
        (sizeof(float) + sizeof(double) + sizeof(uintmax_t)) * size * 2;

    if (*raw == NULL) {
        *raw = malloc(raw_size);
    } else {
        *raw = realloc(*raw, raw_size);
    }
    if (*raw == NULL) {
        (void)fprintf(stderr, "Unable to allocate memory: %zu\n", size);
        exit(EXIT_FAILURE);
    }

    setup_output_pointers(mem, *raw);
    setup_input_pointers(mem, *raw + raw_output_size, size);
}

static void invalidate_data_cache(void)
{
    setup_prandom_chars(dummy_memarea, sizeof(dummy_memarea), 1, false);
    // (void)p_bench_dummy_func(dummy_memarea, sizeof(dummy_memarea));
}

static void item_preface(struct item_data *data,
                         const struct p_bench_item *item)
{
    invalidate_data_cache();

    data->start = platform_clock();
}

static void item_done(struct item_data *data,
                      const struct p_bench_specification *spec,
                      const char *name)
{
    assert(name != NULL);
    assert(name[0] != 0);

    platform_clock_t now = platform_clock();
    (void)printf("%s, %zu, ", name, spec->current_size);
    platform_print_duration(data->start, now);
    (void)printf("\n");
}
