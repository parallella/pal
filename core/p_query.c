/**
 *
 * Queries an opaque object,returning an integer
 *
 * @param obj  Device object to query
 *
 * @param prop Property to query
 *
 *        TYPE     - Device type
 *                   0:EPIPHANY
 *                   1:SMP
 *                   2:FPGA
 *                   3:GPU
 *                   4:GRID
 *        ISA      - Instruction Set Architecture
 *                   0:EPIPHANY
 *                   1:x86
 *                   2:ARM
 *                   3:MIPS
 *        MEMARCH  - Memory architecture
 *                   0:32 bits
 *                   1:64 bits
 *        TOPOLOGY - Network topology of device
 *                   0:Point to point
 *                   1:Bus
 *                   2:Star
 *                   3:Ring
 *                   4:Mesh
 *                   5:Hypercube
 *                   6:Tre
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
 *
 * @return      Returns result of query (int)
 *
 */
#include "pal_core.h"
#include "pal_core_private.h"
#include <stdio.h>
int p_query (void *obj, int prop){
    
    int res;
    p_dev_t *dev;
    /*TODO, need to be clever with queries different object types*/
    dev = (p_dev_t *) obj;    
    res=dev->property[property];
    return (res);
}
