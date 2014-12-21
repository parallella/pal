
.file    "e_ctimer_start.s";

// ------------------------------------------------------------------------
.section .text;
.type    _e_ctimer_start, %function;
.global  _e_ctimer_start;

.balign 4;
_e_ctimer_start:

        and   r0, r0, r0;                    // set the status to check which timer register
    //----
        bne   _ctimer1_start;                // jump to code for timer1


.balign 4;
_ctimer0_start:

        movfs r3, config;                    // get the current config setting
    //----
        mov   r0, %low(0xffffff0f);          // load mask for the config register
    //----
        movt  r0, %high(0xffffff0f);         // load mask for the config register
    //----
        lsl   r1, r1, 0x4;                   // the ctimer0 control bits start at bit4
    //----
        and   r3, r3, r0;                    // apply the mask to clear TIMERMODE bits from previous config setting
    //----
        movts config, r3;                    // turn the ctimer0 off
    //----
        orr   r3, r3, r1;                    // add the desired TIMERMODE to config
    //----
        movts config, r3;                    // start the ctimer counter
    //----
        movfs r0, ctimer0;                   // read the ctimer value
    //----
        rts;                                 // return with the current value of the ctimer


.balign 4;
_ctimer1_start:

        movfs r3, config;                    // get the current config setting
    //----
        mov   r0, %low(0xfffff0ff);          // load mask for the config register
    //----
        movt  r0, %high(0xfffff0ff);         // load mask for the config register
    //----
        lsl   r1, r1, 0x8;                   // the ctimer1 control bits start at bit8
    //----
        and   r3, r3, r0;                    // apply the mask to clear TIMERMODE bits from previous config setting
    //----
        movts config, r3;                    // turn the ctimer1 off
    //----
        orr   r3, r3, r1;                    // add the desired TIMERMODE to config
    //----
        movts config, r3;                    // start the ctimer counter
    //----
        movfs r0, ctimer1;                   // read the ctimer value
    //----
        rts;                                 // return with the current value of the ctimer


.size    _e_ctimer_start, .-_e_ctimer_start;


/* ------------------------------------------------------------------------
   End of File
   ------------------------------------------------------------------------ */

