/**
 *
 * Queries a PAL object,returning an integer
 * The object can be of type pal_dev_t, pal_mem_t, or pal_team_t.
 * Valid queries should never return '0'.
 *
 * @param obj  Device object to query
 *
 * @param prop Property to query
 *
 *        TYPE     - Device type
 *                   1:EPIPHANY
 *                   2:GRID
 *                   3:SMP
 *                   4:FPGA
 *                   5:GPU
 *        ISA      - Instruction Set Architecture
 *                   1:EPIPHANY
 *                   2:x86
 *                   3:ARM
 *                   4:MIPS
 *        MEMARCH  - Memory architecture
 *                   1:32 bits
 *                   2:64 bits
 *        TOPOLOGY - Network topology of device
 *                   1:Point to point
 *                   2:Bus
 *                   3:Star
 *                   4:Ring
 *                   5:Mesh
 *                   6:Hypercube
 *                   7:Tree
 *        NODES    - Number of nodes in device
 *        ROWS     - Number of rows in topology
 *        COLS     - Number of cols in topology
 *        PLANES   - Number of planes in topology
 *        CHIPROWS - Number of rows on a single chip
 *        CHIPCOLS - Number of cols on a single chip
 *        SIMD     - Vector size at each node
 *        MEMSIZE  - Local memory size at each node
 *        MEMBASE  - Memory offset of first node
 *        VERSION  - Unique platform version "tag"
 *        WHOAMI   - Returns index of current node
 *
 * @return      Value of property being queried
 *
 */
#include "pal_base.h"
#include "pal_base_private.h"
#include <stdio.h>
int p_query(p_dev_t dev, int prop)
{
    printf("Running p_query(%ld,%d)\n", dev, prop);

#if 0
    int res;
    struct p_dev *devptr = p_dev_table_global.devptr[dev];
    res = devptr->property[P_QUERY_NODES];
    return (res);
#endif

    return -ENOSYS;
}
