#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

#include <e-hal.h>

static const uint8_t empty[128] = { 0 };

struct status {
    uint32_t done;
    uint32_t _pad1;
    uint32_t nbench;
    uint32_t _pad2;
};

struct result {
    char name[64];
    uint64_t ns;
    uint64_t size;
};

int main(int argc, char **argv, char **envp)
{
    e_platform_t platform;
    e_epiphany_t dev;
    e_mem_t   result_handle;
    e_mem_t   status_handle;
    struct status status;
    struct result *results;
    uint32_t i;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s PROGRAM\n", argv[0]);
        return 1;
    }

    e_init(NULL);
    e_reset_system();
    e_get_platform_info(&platform);

    if (e_open(&dev, 0, 0, platform.rows, platform.cols)) {
        fprintf(stderr, "ERROR: Cannot open device\n");
        exit(EXIT_FAILURE);
    }
    if (e_alloc(&status_handle, 0x01200000, 4096)) {
        fprintf(stderr, "ERROR: Failed allocating buffer\n");
        exit(EXIT_FAILURE);
    }
    if (0 >= e_write(&status_handle, 0, 0, 0, empty, sizeof(empty))) {
        fprintf(stderr, "ERROR: Writing to ERAM failed\n");
        exit(EXIT_FAILURE);
    }
    if (e_alloc(&result_handle, 0x01300000, 1024 * 1024)) {
        fprintf(stderr, "ERROR: Failed allocating buffer\n");
        exit(EXIT_FAILURE);
    }
    if (0 >= e_write(&result_handle, 0, 0, 0, empty, sizeof(empty))) {
        fprintf(stderr, "ERROR: Writing to ERAM failed\n");
        exit(EXIT_FAILURE);
    }
    if (e_load_group(argv[1], &dev, 0, 0, 1, 1, true)) {
        fprintf(stderr, "ERROR: Cannot load binary\n");
        exit(EXIT_FAILURE);
    }

    while (true) {
        if (0 >= e_read(&status_handle, 0, 0, 0x00000000, &status,
            sizeof(status))) {
            exit(EXIT_FAILURE);
        }

        if (status.done)
            break;

        usleep(10000);
    }

    results = calloc(1024 * 1024, 1);
    if (!results) {
        fprintf(stderr, "ERROR: No memory\n");
        exit(EXIT_FAILURE);
    }

    if (0 >= e_read(&result_handle, 0, 0, 0, results,
                sizeof(results[0]) * status.nbench)) {
        fprintf(stderr, "ERROR: Failed reading result\n");
        exit(EXIT_FAILURE);
    }

    e_close(&dev);
    e_finalize();

    printf(";name, size, duration (ns)\n");
    for (i = 0; i < status.nbench; i++)
        printf("%s, %" PRIu64 ", %" PRIu64 "\n",
               results[i].name, results[i].size, results[i].ns);


    free(results);

    return 0;
}
