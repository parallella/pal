#include "../bench_tmpl.h"
void bench_p_rand(const struct p_bench_specification *spec) {
    volatile int r;
    for (size_t i = 0; i < spec->current_size; i++) {
        r = p_rand();
    }
    (void) r;
}
const struct p_bench_item benchmark_items[] = {
    item(p_rand),

    { NULL, NULL }
};
