

.file    "e_ctimer_get.s";


// ------------------------------------------------------------------------
.section .text;
.type    _e_ctimer_get, %function;
.global  _e_ctimer_get;

.balign 4;
_e_ctimer_get:

        and   r0, r0, r0;                    // set the status to check which timer register
    //----
        bne   _ctimer1_get;                  // jump to code for timer1


.balign 4;
_ctimer0_get:

        movfs r0, ctimer0;                   // read the ctimer value
    //----
        rts;                                 // return with the current value of the ctimer


.balign 4;
_ctimer1_get:

        movfs r0, ctimer1;                   // read the ctimer value
    //----
        rts;                                 // return with the current value of the ctimer

.size    _e_ctimer_get, .-_e_ctimer_get;


/* ------------------------------------------------------------------------
   End of File
   ------------------------------------------------------------------------ */

