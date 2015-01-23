/**
 *
 * Closes down a team, freeing any allocated resources.
 *
 * @param team  Pointer to team object
 *
 * @return      Returns 0 if successful.
 *
 */
#include <stdio.h>
#include "pal_core.h"
#include "pal_core_private.h"

int p_close (p_team_t *team){
    printf("Running p_close(p_team_t)\n");
    return(0);
}
