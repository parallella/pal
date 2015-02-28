#include <pal.h>

#include <stdio.h>
#include "pal_base.h"
#include "pal_base_private.h"

/**
 *
 * Create a team of processors based on the the total list of processors
 * defined within the device 'dev'.
 *
 * @param dev   Pointer to object containing device information
 *
 * @param start Index of first processor within 'dev' to include in team
 *
 * @param count Total number of processors in team.
 *
 * @return      Returns a reference. Negative value indicates error.
 *
 */
p_team_t p_open(p_dev_t dev, int start, int count)
{
    struct dev *pdev = (struct dev *) dev;
    struct team *team;

    if (p_ref_is_err(dev))
        return p_ref_err(EINVAL);

    team = malloc(sizeof(*team));
    if (!team)
        return p_ref_err(ENOMEM);

    team = pdev->dev_ops->open(pdev, team, start, count);
    if (p_ref_is_err(team)) {
        free(team);
        goto out;
    }

    /* TODO: Rank ranges */

    /* TODO: Need something generic to iterate over lists */
    if (!__pal_global.teams_head) {
        __pal_global.teams_head = __pal_global.teams_tail = team;
    } else {
        __pal_global.teams_tail->next = team;
        __pal_global.teams_tail = team;
    }

out:
    return (p_team_t) team;

}
