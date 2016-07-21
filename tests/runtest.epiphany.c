#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <pal.h>

/* https://www.gnu.org/software/automake/manual/html_node/Scripts_002dbased-Testsuites.html */
#define HARD_ERROR 99

struct status {
    uint32_t done;
    uint32_t _pad1;
    int32_t  returncode;
    uint32_t _pad2;
} __attribute__((packed));

void usage(char **argv)
{
    fprintf(stderr, "Usage: %s TEST\n", argv[0]);
    exit(HARD_ERROR);
}

int main(int argc, char **argv)
{
    int returncode, err;
    p_mem_t status_mem, results_mem;
    char results[1024] = { '\0' };
    struct status status = { .returncode = HARD_ERROR };
    p_dev_t dev;
    p_prog_t prog;
    p_team_t team;

    if (argc != 2)
        usage(argv);

    dev = p_init(P_DEV_EPIPHANY, 0);
    prog = p_load(dev, argv[1], 0);
    team = p_open(dev, 0, 16); // TODO: Must be 16 for Epiphany

    status_mem = p_map(dev, 0x8f200000, sizeof(status));
    results_mem = p_map(dev, 0x8f300000, sizeof(results));

    /* Clear */
    p_write(&status_mem, &status, 0, sizeof(status), 0);
    p_write(&results_mem, results, 0, sizeof(results), 0);

    err = p_run(prog, "main", team, 0, 1, 0, NULL, 0);

    /* Read back */
    p_read(&status_mem, &status, 0, sizeof(status), 0);
    p_read(&results_mem, results, 0, sizeof(results), 0);

    returncode = err ? HARD_ERROR : status.returncode;

    printf("%s\n", results);

    p_close(team);
    p_finalize(dev);

    return returncode;
}
