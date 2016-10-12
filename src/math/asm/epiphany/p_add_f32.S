// ##########################################################################
// Assembly optimized vector add routine
// ##########################################################################
//
// r0      = arg pointer to a vector
// r1      = arg pointer to b vector
// r2      = arg pointer to c vector
// r3      = arg N (length, multiple of 16)
// r16     = temp variable
// r39:32  = a[7:0]
// r47:40  = a[7:0]
// r55:48  = c[7:0]
//
// ##########################################################################	
.include "epiphany_macros.inc"
.global p_add_f32
p_add_f32:
		ptrace	 			     	// dump PC, timer
		ldrd    r32,[r0],#1			// load a[1:0]
		ldrd    r40,[r1],#1			// load b[1:0]
		lsr	r3,r3,#4			// divide by innter loop chunk
		loop_init func_core,func_end,r3,r16	// init HW loops
		.balignw 8,0x01a2	       		// align to 8-byte boundary
func_core:
		.rept 2
		ldrd    r34,[r0],#1			// load a[3:2]
	        fadd    r48,r32,r40			// c[0]=a[0]+b[0]
		ldrd    r42,[r1],#1			// load b[3:2]
		fadd    r49,r33,r41			// c[1]=a[1]+b[1]
		ldrd    r36,[r0],#1			// load a[5:4]
		fadd    r50,r34,r42			// c[2]=a[2]+b[2]
		ldrd    r44,[r1],#1			// load b[5:4]
		fadd    r51,r35,r43			// c[3]=a[3]+b[3]
		ldrd    r38,[r0],#1			// load a[7:6]
		fadd    r52,r36,r44			// c[4]=a[4]+b[4]
		ldrd    r46,[r1],#1			// load b[7:6]
		fadd    r53,r37,r45			// c[5]=a[5]+b[5]
		strd    r48,[r2],#1			// mem[]=c[1:0] 
		fadd    r54,r38,r46			// c[6]=a[6]+b[6]
	        strd    r50,[r2],#1			// mem[]=c[3:2]  
		fadd    r55,r39,r47			// c[7]=a[7]+b[7]
		ldrd    r32,[r0],#1			// load a[1:0]
		ldrd    r40,[r1],#1			// load b[1:0]
	        strd    r52,[r2],#1			// mem[]=c[5:4] 
	        strd    r54,[r2],#1			// mem[]=c[7:6] 
		.endr
func_end:
		loop_finish func_core			// finish HW loops
		idle
		ptrace					// initialize function
		rts					// return


