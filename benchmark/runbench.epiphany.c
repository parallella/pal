#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>

#include <e-hal.h>

static const uint8_t empty[128] = { 0 };

int main(int argc, char **argv, char **envp)
{
    e_platform_t platform;
    e_epiphany_t dev;
    e_mem_t   stdout_handle;
    e_mem_t   status_handle;
    char *buf;

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
    if (e_alloc(&stdout_handle, 0x01300000, 1024 * 1024)) {
        fprintf(stderr, "ERROR: Failed allocating buffer\n");
        exit(EXIT_FAILURE);
    }
    if (0 >= e_write(&stdout_handle, 0, 0, 0, empty, sizeof(empty))) {
        fprintf(stderr, "ERROR: Writing to ERAM failed\n");
        exit(EXIT_FAILURE);
    }
    if (e_load_group(argv[1], &dev, 0, 0, 1, 1, true)) {
        fprintf(stderr, "ERROR: Cannot load binary\n");
        exit(EXIT_FAILURE);
    }

    while (true) {
        uint32_t done;

        if (0 >= e_read(&status_handle, 0, 0, 0x00000000, &done, sizeof(done))) {
            exit(EXIT_FAILURE);
        }

        if (done)
            break;

        usleep(10000);
    }

    buf = calloc(1024 * 1024, 1);
    if (!buf) {
        fprintf(stderr, "ERROR: No memory\n");
        exit(EXIT_FAILURE);
    }

    if (0 >= e_read(&stdout_handle, 0, 0, 0, buf, 1024)) {
        fprintf(stderr, "ERROR: Failed reading result\n");
        exit(EXIT_FAILURE);
    }

    e_close(&dev);
    e_finalize();

    printf("%s", buf);

    free(buf);

    return 0;
}
