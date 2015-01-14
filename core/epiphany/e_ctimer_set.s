
//unsigned int e_ctimer_set(e_ctimer_id_t timer, unsigned val);

.file    "e_ctimer_set.s";


// ------------------------------------------------------------------------
.section .text;
.type    _e_ctimer_set, %function;
.global  _e_ctimer_set;

.balign 4;
_e_ctimer_set:

        and   r0, r0, r0;                    // set the status to check which timer register
    //----
        bne   _ctimer1_set;                  // jump to code for timer1


.balign 4;
_ctimer0_set:

        mov   r0, r1;                        // set the return value
    //----
        movts ctimer0, r1;                   // set the ctimer counter to the desired value
    //----
        rts;                                 // return with the current value of the ctimer


.balign 4;
_ctimer1_set:

        mov   r0, r1;                        // set the return value
    //----
        movts ctimer1, r1;                   // set the ctimer counter to the desired value
    //----
        rts;                                 // return with the current value of the ctimer


.size    _e_ctimer_set, .-_e_ctimer_set;


/* ------------------------------------------------------------------------
   End of File
   ------------------------------------------------------------------------ */

