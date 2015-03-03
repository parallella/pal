/*

Copyright (C) 2012 Adapteva, Inc.
Contributed by Andreas Olofsson <support@adapteva.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program, see the file COPYING. If not, see
<http://www.gnu.org/licenses/>.
*/

/*

The purpose of the program is to demonstrate simple parallel programming 
example for the Parallella demonstrating how to do fork-join 
parallel processing using the Epiphany library. Similar type programs can be 
constructed with OpenCL, OpenMP, and many other parallel programming frameworks.


The main.c program distributes N tasks to N Epiphany cores in parallel and
aggregates the result after all cores have completed to produce the final 
result.
   
*/

#include <stdlib.h>
#include <stdio.h>
#include <e-hal.h>
#include "common.h"

int main(int argc, char *argv[]){
  e_platform_t platform;
  e_epiphany_t dev;

  int a[N], b[N], c[CORES];
  int done[CORES],all_done;
  int sop;
  int i,j;
  unsigned clr;
  clr = (unsigned)0x00000000;

  //Calculation being done
  printf("Calculating sum of products of two integer vectors of length %d initalized to all 0x1's using %d Ccores.\n",N,CORES);
  printf("........\n");

  //Initalize Epiphany device
  e_init(NULL);                      
  e_reset_system();                                      //reset Epiphany
  e_get_platform_info(&platform);
  e_open(&dev, 0, 0, platform.rows, platform.cols); //open all cores

  //Initialize a/b input vectors on host side  
  for (i=0; i<N; i++){
    a[i] = 1;
    b[i] = 1;	  
  }
    
  //1. Copy data (N/CORE points) from host to Epiphany local memory
  //2. Clear the "done" flag for every core
  for (i=0; i<platform.rows; i++){
    for (j=0; j<platform.cols;j++){
      e_write(&dev, i, j, 0x2000, &a, (N/CORES)*sizeof(int));
      e_write(&dev, i, j, 0x4000, &b, (N/CORES)*sizeof(int));
      e_write(&dev, i, j, 0x7000, &clr, sizeof(clr));

    }
  }
  //Load program to cores and run
  e_load_group("e_task.srec", &dev, 0, 0, platform.rows, platform.cols, E_TRUE);
  
  //Check if all cores are done
  while(1){    
    all_done=0;
    for (i=0; i<platform.rows; i++){
      for (j=0; j<platform.cols;j++){
	e_read(&dev, i, j, 0x7000, &done[i*platform.cols+j], sizeof(int));
	all_done+=done[i*platform.cols+j];
      }
    }
    if(all_done==16){
      break;
    }
  }

  //Copy all Epiphany results to host memory space
  for (i=0; i<platform.rows; i++){
      for (j=0; j<platform.cols;j++){
	e_read(&dev, i, j, 0x6000, &c[i*platform.cols+j], sizeof(int));
      }
  }

  //Calculates final sum-of-product using Epiphany results as inputs
  sop=0;
  for (i=0; i<CORES; i++){
    sop += c[i];
  }

  //Print out result
  printf("Sum of Product Is %d!\n",sop);
  fflush(stdout);
  //Close down Epiphany device
  e_close(&dev);
  e_finalize();

  if(sop==4096){
    return EXIT_SUCCESS;
  }
  else{
    return EXIT_FAILURE;
  }
}

