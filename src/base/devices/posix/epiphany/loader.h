#pragma once
struct team;
struct prog;

extern int epiphany_load(struct team *team, int start, int count,
                         struct prog *prog, const char *function,
                         int argn, const p_arg_t *args);
extern void epiphany_start(struct team *team, int start, int count);
extern int epiphany_soft_reset(struct team *team, int start, int count);
int epiphany_reset_system(struct epiphany_dev *epiphany);
extern bool epiphany_is_core_done (struct team *team, int rank);
