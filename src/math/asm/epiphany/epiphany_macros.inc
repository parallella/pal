//#####################################
// HARDWARE LOOPS
//#####################################
.macro loop_init start, end, reg_count, reg_tmp
       mov	\reg_tmp, %low(\start)
       movts	ls, \reg_tmp
       mov	\reg_tmp, %low(\end - 4)
       movts	le, \reg_tmp
       movts 	lc, \reg_count
       gid
.endm

.macro loop_finish start
       gie
.endm

//#####################################
// PROFILING
//#####################################
.macro ptrace
.endm


