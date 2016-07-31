#include <string.h>

#include <pal.h>
#include "pal_base_private.h"

p_team_t p_open4(p_dev_t dev, int topology, p_coords_t *start,
                 p_coords_t *size)
{
    struct dev *pdev = (struct dev *) dev;
    struct team *team, *ret;
    int err;

    if (p_ref_is_err(dev))
        return p_ref_err(EINVAL);

    team = malloc(sizeof(*team));
    if (!team)
        return p_ref_err(ENOMEM);

    team->dev = dev;
    team->topology = topology;
    memcpy(&team->start, start, sizeof(*start));
    memcpy(&team->size, size, sizeof(*size));
    switch (topology) {
    case P_TOPOLOGY_FLAT:
        team->rank.id = -1;
        break;
    case P_TOPOLOGY_3D:
        team->rank.plane = -1;
        /* Fall through */
    case P_TOPOLOGY_2D:
        team->rank.row = -1;
        team->rank.col = -1;
        break;
    default:
        err = EINVAL;
        goto error;
    }

    ret = pdev->dev_ops->open(team);
    if (p_error(ret)) {
        err = p_error(ret);
        goto error;
    }

    team = ret;

    /* TODO: Need something generic to iterate over lists */
    if (!__pal_global.teams_head) {
        __pal_global.teams_head = __pal_global.teams_tail = team;
    } else {
        __pal_global.teams_tail->next = team;
        __pal_global.teams_tail = team;
    }

    return (p_team_t) team;

error:
    free(team);
    return p_ref_err(err);
}

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
    p_coords_t start_coords = { .id = start };
    p_coords_t size_coords = { .id = count };

    return p_open4(dev, P_TOPOLOGY_FLAT, &start_coords, &size_coords);
}
