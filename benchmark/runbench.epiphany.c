#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

#include <pal.h>

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

void usage(char **argv)
{
    fprintf(stderr, "Usage: %s PROGRAM\n", argv[0]);
    exit(1);
}

int main(int argc, char **argv)
{
    int returncode, err, i;
    struct status status = { 0 };
    struct result *results;
    uint8_t clear[1024 * 1024] = { 0 };
    p_mem_t status_mem, results_mem;
    p_dev_t dev;
    p_prog_t prog;
    p_team_t team;

    if (argc != 2)
        usage(argv);

    dev = p_init(P_DEV_EPIPHANY, 0);
    prog = p_load(dev, argv[1], 0);
    team = p_open(dev, 0, 1);

    status_mem = p_map(dev, 0x8f200000, sizeof(status));
    results_mem = p_map(dev, 0x8f300000, 1024 * 1024);

    /* Clear */
    p_write(&status_mem, &status, 0, sizeof(status), 0);
    p_write(&results_mem, clear, 0, sizeof(clear), 0);

    err = p_run(prog, "main", team, 0, 1, 0, NULL, 0);

    /* Read back */
    p_read(&status_mem, &status, 0, sizeof(status), 0);
    results = alloca(sizeof(*results) * status.nbench);
    p_read(&results_mem, results, 0, sizeof(*results) * status.nbench, 0);

    printf(";name, size, duration (ns)\n");
    for (i = 0; i < status.nbench; i++)
        printf("%s, %" PRIu64 ", %" PRIu64 "\n",
               results[i].name, results[i].size, results[i].ns);

    p_close(team);
    p_finalize(dev);

    return 0;
}
