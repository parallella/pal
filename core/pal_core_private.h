#include "pal_core.h"

struct pal_dev {

    int name;
    int whoami;
    int topology;
    int nodes;
    int origin;
    int rows;
    int cols;
    int chip_cols;
    int chip_rows;
    int loc_memsize;
    int sh_memsize;
};

struct pal_team {
    int origin;
    int rows;
    int cols;
};

struct pal_symbol {

};

struct pal_mem {

};

struct pal_program {

};

struct pal_event {

};

struct pal_atom {

};

struct pal_mutex {

};
