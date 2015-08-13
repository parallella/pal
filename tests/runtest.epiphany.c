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

/* https://www.gnu.org/software/automake/manual/html_node/Scripts_002dbased-Testsuites.html */
#define HARD_ERROR 99

struct status {
    uint32_t done;
    uint32_t _pad1;
    uint32_t returncode;
    uint32_t _pad2;
} __attribute__((packed));

int main(int argc, char **argv, char **envp)
{
    e_platform_t platform;
    e_epiphany_t dev;
    e_mem_t   result_handle;
    e_mem_t   status_handle;
    struct status status;
    char *results;
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
        exit(HARD_ERROR);
    }
    if (e_alloc(&status_handle, 0x01200000, 4096)) {
        fprintf(stderr, "ERROR: Failed allocating buffer\n");
        exit(HARD_ERROR);
    }
    if (0 >= e_write(&status_handle, 0, 0, 0, empty, sizeof(empty))) {
        fprintf(stderr, "ERROR: Writing to ERAM failed\n");
        exit(HARD_ERROR);
    }
    if (e_alloc(&result_handle, 0x01300000, 1024 * 1024)) {
        fprintf(stderr, "ERROR: Failed allocating buffer\n");
        exit(HARD_ERROR);
    }
    if (0 >= e_write(&result_handle, 0, 0, 0, empty, sizeof(empty))) {
        fprintf(stderr, "ERROR: Writing to ERAM failed\n");
        exit(HARD_ERROR);
    }
    if (e_load_group(argv[1], &dev, 0, 0, 1, 1, true)) {
        fprintf(stderr, "ERROR: Cannot load binary\n");
        exit(HARD_ERROR);
    }

    while (true) {
        if (0 >= e_read(&status_handle, 0, 0, 0x00000000, &status,
            sizeof(status))) {
            exit(HARD_ERROR);
        }

        if (status.done)
            break;

        usleep(10000);
    }

    results = calloc(1024 * 1024, 1);
    if (!results) {
        fprintf(stderr, "ERROR: No memory\n");
        exit(HARD_ERROR);
    }

    if (0 >= e_read(&result_handle, 0, 0, 0, results, 1024)) {
        fprintf(stderr, "ERROR: Failed reading result\n");
        exit(HARD_ERROR);
    }

    e_close(&dev);
    e_finalize();

    printf("%s\n", results);

    free(results);

    return status.returncode;
}
