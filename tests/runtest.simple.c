#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv, char **envp)
{
    int ret;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s program [ARGUMENTS]\n", argv[0]);
        return 1;
    }

    ret = execve(argv[1], &argv[1], envp);

    fprintf(stderr, "ERROR: %s: execve failed: %d\n", argv[0], ret);

    return 1;
}
