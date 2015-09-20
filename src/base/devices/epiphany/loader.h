#pragma once
struct team;
struct prog;
extern int epiphany_load(struct team *team, struct prog *prog,
                         int start, int size, int flags);

extern void epiphany_start(struct team *team, int start, int size, int flags);
extern void epiphany_soft_reset(struct team *team, int start, int size);
