/*
 ***********************************************************************
 * PROGRAM LOADER AND RUN
 *
 ***********************************************************************
 *
 *  Goals:
 * -Support multiple device types (ARM, FPGA, Epiphany, GPU) 
 * -Supports multiple linux processes interacting with a device independently
 * -Supports a group of cooperating threads within a group within a process
 * (only possible of there are multiple processes, coprocessor model)
 */

/*Initializes the PAL run time*/
/*This should create an opaque structure)*/
/*Assume you can have multiple devices*/
/*This has to be here, don't assume O/S support*/
/*ARM ASYNC
  ARM+FPGA (same interface)
  This should return a set of IDs
  There needs to be some information about topology as well
  0..N
  N x M
  N x M x K
  this can be done in open, request as 2D array, 1 D array
*/ 

/*connects to a device*/
/*turns it on if it was off*/
/*issues reset if we are the first ones*/
/*where to read the setup information from*/
pal_dev_t *p_init(int flags);

/*Opens connection to a set of slave processors*/
pal_team_t *p_open(pal_dev_t *dev, int nstart, int ntotal, int ncols );

/*
 *Loads a program file into memory from the file system
 *could be a bit stream, elf, etc
*/
pal_program_t *p_load(char *executable);

/*Run a program on N processors of a device*/
void p_run (pal_program_t *prog, //program to run
            pal_team_t *team,    //team of processors to run on
            int np,              //processor offset within team (0..N-1),  
            int nargs,           //number of program arguments
            void** args,         //array of pointers to program arguments
            int flags            //launch flags ("ALL"), default is one
           );


/*Notifies run time, that you are done with HW
 *Used for power saving
 */

void p_finalize(pal_dev_t);
