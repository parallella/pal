/*
 *
 * FUNCTION:
 * p_load(): Loads a program file into memory from the file system
 *           could be a bit stream, elf, etc
 *            
 * ARGUMENTS:
 * dev    - device descriptor indiciating the type of device to run on
 * exe    - executable to run (file path)
 *
 * RETURN VALUE:
 *
 */

pal_program_t p_load (pal_dev_t *dev, char *exe){

    /*PLACE CODE HERE*/   

    //1.Parse file depending on type of device (ELF, bit stream
    //2.Place the file into memory, create structure that p_run can use 

}
