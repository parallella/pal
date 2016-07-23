#include <pal.h>

/**
 *
 * Send signal to team members
 *
 * @param team      Team to kill with.
 *
 * @param start     Relative starting processor within team
 *
 * @param size      Total number of processors within team to kill
 *
 * @param signal    POSIX signal (SIGKILL, SIGTERM, ...)
 *
 * @return          Returns 0 if successful.
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "pal_base.h"
#include "pal_base_private.h"

int p_kill(p_team_t team, int start, int count, int signal)
{
    struct team *pteam = (struct team *) team;

    return pteam->dev->dev_ops->kill(pteam, start, count, signal);
}
