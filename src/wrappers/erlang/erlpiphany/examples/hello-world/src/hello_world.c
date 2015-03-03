/*
  hello_world.c

  Copyright (C) 2012 Adapteva, Inc.
  Contributed by Yaniv Sapir <yaniv@adapteva.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program, see the file COPYING.  If not, see
  <http://www.gnu.org/licenses/>.
*/

// This is the HOST side of the Hello World example.
// The program initializes the Epiphany system,
// randomly draws an eCore and then loads and launches
// the device program on that eCore. It then reads the
// shared external memory buffer for the core's output
// message.

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <e-hal.h>

#define _BufSize   (128)
#define _BufOffset (0x01000000)
#define _SeqLen    (20)

int main(int argc, char *argv[])
{
	unsigned row, col, coreid, i;
	e_platform_t platform;
	e_epiphany_t dev;
	e_mem_t emem;
	char emsg[_BufSize];

	srand(1);

	// initialize system, read platform params from
	// default HDF. Then, reset the platform and
	// get the actual system parameters.
	e_init(NULL);
	e_reset_system();
	e_get_platform_info(&platform);

	// Allocate a buffer in shared external memory
	// for message passing from eCore to host.
	e_alloc(&emem, _BufOffset, _BufSize);

	for (i=0; i<_SeqLen; i++)
	{
		// Draw a random core
		row = rand() % platform.rows;
		col = rand() % platform.cols;
		coreid = (row + platform.row) * 64 + col + platform.col;
		fprintf(stderr, "%3d: Message from eCore 0x%03x (%2d,%2d): ", i, coreid, row, col);

		// Open the single-core workgroup and reset the core, in
		// case a previous process is running. Note that we used
		// core coordinates relative to the workgroup.
		e_open(&dev, row, col, 1, 1);
		e_reset_group(&dev);

		// Load the device program onto the selected eCore
		// and launch after loading.
		e_load("e_hello_world.srec", &dev, 0, 0, E_TRUE);

		// Wait for core program execution to finish, then
		// read message from shared buffer.
		usleep(10000);
		e_read(&emem, 0, 0, 0x0, emsg, _BufSize);

		// Print the message and close the workgroup.
		fprintf(stderr, "\"%s\"\n", emsg);
		e_close(&dev);
	}

	// Release the allocated buffer and finalize the
	// e-platform connection.
	e_free(&emem);
	e_finalize();

	return 0;
}

