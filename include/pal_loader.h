/*
 ***********************************************************************
 * PROGRAM LOADER/EXECUTION
 *
 ***********************************************************************
 */

/*Initializes the PAL run time*/
void p_init();

/*Loads a binary elf file into an array and prepares it for execution*/
void p_load();

/*Opens connection to a set of slave processors*/
void p_open();

/*Launches the program on a set of processors*/
void p_exec();

/*Closes connection to a set of slave processors*/
void p_close();




