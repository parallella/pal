#pragma once
struct team;
struct prog;

extern int epiphany_load(struct team *team, struct prog *prog,
                         int start, int size, int flags, int argn,
                         const p_arg_t *args, const char *function);
extern void epiphany_start(struct team *team, int start, int size, int flags);
extern int epiphany_soft_reset(struct team *team, int start, int size);
int epiphany_reset_system(struct epiphany_dev *epiphany);
