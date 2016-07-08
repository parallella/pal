#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <pal.h>

/* https://www.gnu.org/software/automake/manual/html_node/Scripts_002dbased-Testsuites.html */
#define HARD_ERROR 99

struct status {
    uint32_t done;
    uint32_t _pad1;
    uint32_t returncode;
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
    char *results;
    struct status *status;
    p_dev_t dev;
    p_prog_t prog;
    p_team_t team;

    if (argc != 2)
        usage(argv);

    dev = p_init(P_DEV_EPIPHANY, 0);
    prog = p_load(dev, argv[1], 0);
    team = p_open(dev, 0, 16); // TODO: Must be 16 for Epiphany

    status = p_map(dev, 0x8f200000, sizeof(*status));
    results = p_map(dev, 0x8f300000, 1024);

    memset(status, 0, sizeof(*status));
    memset(results, 0, 1024);

    err = p_run(prog, "main", team, 0, 1, 0, NULL, 0);
    returncode = err ? HARD_ERROR : status->returncode;

    printf("%s\n", results);

    p_close(team);
    p_finalize(dev);

    return returncode;
}
