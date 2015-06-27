
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <errno.h>

#include "wrapper.h"

/* The ratio of the largest known output to input
 * e.g.: p_conv_f32 -> nr+nh-1 -> ratio 2
 */
static const size_t max_output = 3;

/* ifdef posix
 * this prototype is posix specific
 */

#include <sys/times.h>

typedef clock_t platform_clock_t;

static platform_clock_t platform_clock(void)
{
    struct tms tmsbuffer;

    (void)times(&tmsbuffer);
    return tmsbuffer.tms_utime;
}

static void platform_print_duration(platform_clock_t start,
                                    platform_clock_t end)
{
    printf("%ju", (uintmax_t)(end - start));
}

/* end of platform specific section */

struct item_data
{
    platform_clock_t start;
};

static void item_preface(struct item_data *, const struct p_benchmark_item *);
static void item_done(struct item_data *,
                      const struct p_benchmark_specification *, const char *);
static void setup_memory(struct p_benchmark_raw_memory *, char **raw, size_t);

static char dummy_memarea[1024 * 1024 * 32];
int p_benchmark_dummy_func(char *, size_t);

int main(void)
{
    static const size_t default_initial_size = 655360;

    struct p_benchmark_specification spec;
    char *raw_mem = NULL;
    spec.current_size = default_initial_size;

    setup_memory(&spec.mem, &raw_mem, spec.current_size);
    for (const struct p_benchmark_item *item = benchmark_items;
         item->name != NULL; ++item) {
        struct item_data data;

        item_preface(&data, item);
        item->benchmark(&spec);
        item_done(&data, &spec, item->name);
    }
    return EXIT_SUCCESS;
}

static void setup_output_pointers(struct p_benchmark_raw_memory *mem, void *p)
{
    mem->output_float = p;
    mem->output_double = p;
    mem->output_uintmax_t = p;
    mem->output_char = p;
    mem->output_short = p;
    mem->output_int = p;
    mem->output_long = p;
    mem->output_uint16_t = p;
    mem->output_uint32_t = p;
    mem->output_uint64_t = p;
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

static void copy_integral_input_pointers(struct p_benchmark_raw_memory *mem)
{
    mem->input_char_first = (char *)mem->input_uintmax_t_first;
    mem->input_char_second = (char *)mem->input_uintmax_t_second;
    mem->input_short_first = (short *)mem->input_uintmax_t_first;
    mem->input_short_second = (short *)mem->input_uintmax_t_second;
    mem->input_int_first = (int *)mem->input_uintmax_t_first;
    mem->input_int_second = (int *)mem->input_uintmax_t_second;
    mem->input_long_first = (long *)mem->input_uintmax_t_first;
    mem->input_long_second = (long *)mem->input_uintmax_t_second;
    mem->input_uint16_t_first = (uint16_t *)mem->input_uintmax_t_first;
    mem->input_uint16_t_second = (uint16_t *)mem->input_uintmax_t_second;
    mem->input_uint32_t_first = (uint32_t *)mem->input_uintmax_t_first;
    mem->input_uint32_t_second = (uint32_t *)mem->input_uintmax_t_second;
    mem->input_uint64_t_first = (uint64_t *)mem->input_uintmax_t_first;
    mem->input_uint64_t_second = (uint64_t *)mem->input_uintmax_t_second;
}

static void setup_input_pointers(struct p_benchmark_raw_memory *mem, char *p,
                                 size_t size)
{
    unsigned seed = 0;

    setup_prandom_chars(p, size * sizeof(float), seed, false);
    mem->input_float_first = (float *)p;
    p += size * sizeof(float);
    setup_prandom_chars(p, size * sizeof(float), seed, true);
    mem->input_float_second = (float *)p;
    p += size * sizeof(float);

    setup_prandom_chars(p, size * sizeof(double), seed, false);
    mem->input_double_first = (double *)p;
    p += size * sizeof(double);
    setup_prandom_chars(p, size * sizeof(double), seed, true);
    mem->input_double_second = (double *)p;
    p += size * sizeof(double);

    setup_prandom_chars(p, size * sizeof(uintmax_t), seed, false);
    mem->input_uintmax_t_first = (uintmax_t *)p;
    p += size * sizeof(uintmax_t);
    setup_prandom_chars(p, size * sizeof(uintmax_t), seed, true);
    mem->input_uintmax_t_second = (uintmax_t *)p;

    copy_integral_input_pointers(mem);
}

static void setup_memory(struct p_benchmark_raw_memory *mem, char **raw,
                         size_t size)
{
    assert(mem != NULL);
    assert(size > 0);
    assert(raw != NULL);

    size_t raw_output_size = sizeof(uintmax_t) * size * max_output;
    size_t raw_size =
        raw_output_size +
        (sizeof(float) + sizeof(double) + sizeof(uintmax_t)) * size * 2;

    printf("raw_size: %zu\n", raw_size);
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
    (void)p_benchmark_dummy_func(dummy_memarea, sizeof(dummy_memarea));
}

static void item_preface(struct item_data *data,
                         const struct p_benchmark_item *item)
{
    invalidate_data_cache();

    data->start = platform_clock();
}

static void item_done(struct item_data *data,
                      const struct p_benchmark_specification *spec,
                      const char *name)
{
    assert(name != NULL);
    assert(name[0] != 0);

    platform_clock_t now = platform_clock();
    (void)printf("%s size: %zu duration: ", name, spec->current_size);
    platform_print_duration(data->start, now);
    (void)printf("\n");
}
