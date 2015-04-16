/*********************************************************************
 @author  Mark A Fleming  <mark_fleming@ieee.org>
 @copyright (C) 2015
 @version 1.0.1
 @doc
 The Erlang Epiphany eHAL NIF loadable library
 Compile using the command
 gcc -o ehal.so -fpic -shared ehal.c \
 -I${EPIPHANY_HOME}/tools/host/include	\
 -L${EPIPHANY_HOME}/tools/host/lib -le-hal

 @end
 Created : 12 Jan 2015 by  Mark A Fleming

Copyright (c) 2015, Mark A Fleming
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met: 

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the <ORGANIZATION> nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*********************************************************************/

#include "erl_nif.h"
#include <string.h>

// SDK includes
#include "e-hal.h"
#include "e-loader.h"

// References to SDK functions
// 14.2 Platform Configuration Functions
extern int e_init(char *hdf);
extern int e_get_platform_info(e_platform_t *platform);
extern int e_finalize(void);

//14.3 Workgroup and External Memory Allocation Functions
extern int e_open(e_epiphany_t *dev, unsigned row, unsigned col,
		  unsigned rows, unsigned cols);
extern int e_close(e_epiphany_t *dev);
extern int e_alloc(e_mem_t *mbuf, off_t base, size_t size);
extern int e_free(e_mem_t *mbuf);

//14.4 Data Transfer Functions
extern ssize_t e_read(void *dev, unsigned row, unsigned col,
		      off_t from_addr, void *buf, size_t size);
extern ssize_t e_write(void *dev, unsigned row, unsigned col,
		       off_t to_addr, const void *buf, size_t size);

//14.5 System Control Functions
extern int e_reset_system();
extern int e_reset_group(e_epiphany_t *dev);
extern int e_start(e_epiphany_t *dev, unsigned row, unsigned col);
extern int e_start_group(e_epiphany_t *dev);
extern int e_signal(e_epiphany_t *dev, unsigned row, unsigned col);
extern int e_halt(e_epiphany_t *dev, unsigned row, unsigned col);
extern int e_resume(e_epiphany_t *dev, unsigned row, unsigned col);

// 14.6 Program Load Functions
extern int e_load(const char *executable, e_epiphany_t *dev, unsigned row,
		  unsigned col, e_bool_t start);
extern int e_load_group(const char *executable, e_epiphany_t *dev, unsigned row,
			unsigned col, unsigned rows, unsigned cols,
			e_bool_t start);

// 14.7 Utility Functions
extern unsigned e_get_num_from_coords(e_epiphany_t *dev, unsigned row,
				      unsigned col);
extern void e_get_coords_from_num(e_epiphany_t *dev, unsigned corenum,
				  unsigned *row, unsigned *col);
extern e_bool_t e_is_addr_on_chip(void *addr);
extern e_bool_t e_is_addr_on_group(e_epiphany_t *dev, void *addr);
extern e_hal_diag_t e_set_host_verbosity(e_hal_diag_t verbose);
extern e_loader_diag_t e_set_loader_verbosity(e_loader_diag_t verbose);


/*********************************************************************
  Chapter 14.2 Platform Configuration Functions
  e_init, e_get_platform_info, e_finalize
*********************************************************************/

/*--------------------------------------------------------------------
  Epiphany SDK e_init function.
  int e_init(char *hdf);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_init_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  char hdf[255];
  int  hdf_int, ret;

  if (!enif_get_string(env, argv[0], hdf, 255, ERL_NIF_LATIN1)) {
    if (enif_get_int(env, argv[0], &hdf_int)){
      if (hdf_int == 0){
	ret = e_init(NULL);
      } else {
	return enif_make_badarg(env);
      }
    } else {
	return enif_make_badarg(env);
    }
  } else {
    ret = e_init(hdf);
  }

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_get_platform_info function.
  int int e_get_platform_info(e_platform_t *platform);
  Validate all inputs before function call.
  Return Value:
  {ok, [ {attribute, Value} ... ]} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_get_platform_info_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_platform_t platform;
  ERL_NIF_TERM version, row, col, rows, cols, regs_base, num_chips;
  ERL_NIF_TERM num_emems, result;
  int  ret;

  ret = e_get_platform_info(&platform);

  switch(ret) {
  case E_OK:
    // Select e_platform_t members
    // @todo Include e_chip_t and e_memseg_t structures as sublists
    version = enif_make_string(env, platform.version, ERL_NIF_LATIN1);
    regs_base = enif_make_int(env, platform.regs_base);
    row = enif_make_int(env, platform.row);
    col = enif_make_int(env, platform.col);
    rows = enif_make_int(env, platform.rows);
    cols = enif_make_int(env, platform.cols);
    num_chips = enif_make_int(env, platform.num_chips);
    num_emems = enif_make_int(env, platform.num_emems);
    result = enif_make_list(env, 8,
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "row"),
					    row),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "col"),
					    col),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "rows"),
					    rows),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "cols"),
					    cols),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "regs_base"),
					    regs_base),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "version"),
					    version),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "num_chips"),
					    num_chips),
			    enif_make_tuple(env, 2,
					    enif_make_atom(env, "num_emems"),
					    num_emems));
      return(enif_make_tuple(env, 2,
			     enif_make_atom(env, "ok"),
			     result)
	     );
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_finalize function.
  int e_finalize();
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_finalize_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  int  ret;

  ret = e_finalize();

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*********************************************************************
  Chapter 14.3 Workgroup and External Memory Allocation Functions
  e_open, e_close, e_alloc, e_free
*********************************************************************/


/*--------------------------------------------------------------------
  Epiphany SDK e_open function.
  int e_open(e_epiphany_t *dev, unsigned row, unsigned col,
		  unsigned rows, unsigned cols);
  Validate all inputs before function call.
  Return Value:
  {ok, Dev :: binary()} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_open_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  unsigned row, col, rows, cols;
  int  ret;
  ErlNifBinary dev_bin;

  if (!enif_get_int(env, argv[0], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &col)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &rows)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[3], &cols)) {
    return enif_make_badarg(env);
  }
  
  ret = e_open(&dev, row, col, rows, cols);

  switch(ret){
  case E_OK:
    enif_alloc_binary(sizeof(dev), &dev_bin);
    memcpy(dev_bin.data, &dev, sizeof(dev));
    return(enif_make_tuple(env, 2,
			   enif_make_atom(env, "ok"),
			   enif_make_binary(env, &dev_bin)
			   )
	   );
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_close function.
  int e_close(e_epiphany_t *dev);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_close_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  int  ret;
  ErlNifBinary dev_bin;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }

  ret = e_close(&dev);
  
  switch(ret){
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_alloc function.
  int e_alloc(e_mem_t *mbuf, off_t base, size_t size);
  Validate all inputs before function call.
  Return Value:
  {ok, Mbuf :: binary()} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_alloc_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_mem_t mbuf;
  off_t base;
  size_t size;
  int  ret;
  ErlNifBinary mem_bin;

  if (!enif_get_int(env, argv[0], (int *)&base)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &size)) {
    return enif_make_badarg(env);
  }
  
  ret = e_alloc(&mbuf, base, size);

  switch(ret){
  case E_OK:
    enif_alloc_binary(sizeof(mbuf), &mem_bin);
    memcpy(mem_bin.data, &mbuf, sizeof(mbuf));
    return(enif_make_tuple(env, 2,
			   enif_make_atom(env, "ok"),
			   enif_make_binary(env, &mem_bin)
			   )
	   );
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_free function.
  int e_free(e_mem_t *mbuf);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_free_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_mem_t mbuf;
  int  ret;
  ErlNifBinary mem_bin;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &mem_bin)) {
      if ((mem_bin.size==sizeof(mbuf)) && (mem_bin.data != 0)) {
	memcpy(&mbuf, mem_bin.data, mem_bin.size);
	// This causes a Segmentation fault
	ret = e_free(&mbuf);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  
  switch(ret){
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*********************************************************************
  Chapter 14.4 Data Transfer Functions
  e_read, e_write
*********************************************************************/


/*--------------------------------------------------------------------
  Epiphany SDK e_read function.
  ssize_t e_read(void *dev, unsigned row, unsigned col,
  off_t from_addr, void *buf, size_t size);
  Validate all inputs before function call.
  Return Value:
  {ok, {Data :: binary(), Ssize :: integer()}} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_read_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  e_mem_t mbuf;
  char *buf;
  unsigned row, col;
  off_t from_addr;
  size_t size, ssize;
  ErlNifBinary dev_bin, buf_bin;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if (!((dev_bin.size==sizeof(dev)) || (dev_bin.size==sizeof(mbuf))) &&
	  (dev_bin.data != 0)) {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[3], (int *)&from_addr)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[4], &size)) {
    return enif_make_badarg(env);
  }
  //buf = (char *)malloc(size);
  enif_alloc_binary(size, &buf_bin);

  if (dev_bin.size==sizeof(dev)) {
    //memcpy(&dev, dev_bin.data, dev_bin.size);
    //ssize = e_read(&dev, row, col, from_addr, buf, size);
    //ssize = e_read(dev_bin.data, row, col, from_addr, buf, size);
    ssize = e_read(dev_bin.data, row, col, from_addr, buf_bin.data, size);
  } else {
    //memcpy(&mbuf, dev_bin.data, dev_bin.size);
    //ssize = e_read(&mbuf, row, col, from_addr, buf, size);
    //ssize = e_read(dev_bin.data, row, col, from_addr, buf, size);
    ssize = e_read(dev_bin.data, row, col, from_addr, buf_bin.data, size);
  }
  
  switch(ssize) {
  case E_ERR:
    enif_release_binary(&buf_bin);
    return(enif_make_atom(env, "error"));
    break;
  default:
    //enif_alloc_binary(ssize, &buf_bin);
    //memcpy(buf_bin.data, buf, ssize);
    //free(buf);
    return(enif_make_tuple(env, 2,
			   enif_make_atom(env, "ok"),
			   enif_make_tuple(env, 2,
					   enif_make_int(env, ssize),
					   enif_make_binary(env, &buf_bin)
					   )
			   )
	   );
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_write function.
  ssize_t e_write(void *dev, unsigned row, unsigned col,
  off_t to_addr, void *buf, size_t size);
  Validate all inputs before function call.
  Return Value:
  {ok, Ssize :: integer()} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_write_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  e_mem_t mbuf;
  unsigned row, col;
  off_t to_addr;
  size_t size, ssize;
  ErlNifBinary dev_bin, buf_bin;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if (!((dev_bin.size==sizeof(dev)) || (dev_bin.size==sizeof(mbuf))) &&
	  (dev_bin.data != 0)) {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[3], (int *)&to_addr)) {
    return enif_make_badarg(env);
  }
  if (!enif_is_binary(env, argv[4])) {
    return enif_make_badarg(env);
  } else {
    if (!enif_inspect_binary(env, argv[4], &buf_bin)) {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[5], &size)) {
    return enif_make_badarg(env);
  }

  ssize = e_write(dev_bin.data, row, col, to_addr, buf_bin.data, buf_bin.size);
  
  switch(ssize) {
  case E_ERR:
    return(enif_make_atom(env, "error"));
    break;
  default:
    return(enif_make_tuple(env, 2,
			   enif_make_atom(env, "ok"),
			   enif_make_int(env, ssize)
			   )
	   );
    break;
  }
}


/*********************************************************************
  Chapter 14.5 System Control Functions
  e_reset_system, e_reset_group, e_start, e_start_group, e_signal
  e_halt, e_resume
*********************************************************************/


/*--------------------------------------------------------------------
  Epiphany SDK e_reset_system function.
  int e_reset_system();
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_reset_system_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  int  ret;

  ret = e_reset_system();

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_reset_group function.
  int e_reset_group(e_epiphany_t *dev);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_reset_group_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }

  ret = e_reset_group(&dev);
  
  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_start function.
  int e_start(e_epiphany_t *dev, unsigned row, unsigned col);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_start_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }

  ret = e_start(&dev, row, col);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_start_group function.
  int e_start(e_epiphany_t *dev);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_start_group_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }

  ret = e_start_group(&dev);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_signal function.
  int e_signal(e_epiphany_t *dev, unsigned row, unsigned col);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_signal_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }

  // This causes a Segmentation fault
  ret = e_signal(&dev, row, col);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_halt function.
  int e_halt(e_epiphany_t *dev, unsigned row, unsigned col);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_halt_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }

  ret = e_halt(&dev, row, col);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_resume function.
  int e_resume(e_epiphany_t *dev, unsigned row, unsigned col);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_resume_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col;
  int  ret;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }

  ret = e_resume(&dev, row, col);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*********************************************************************
  Chapter 14.6 Program Load Functions
  e_load, e_load_group
*********************************************************************/


/*--------------------------------------------------------------------
  Epiphany SDK e_load function.
  int e_load(char *executable, e_epiphany_t *dev, unsigned row,
  unsigned col, e_bool_t start);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_load_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  char *executable, buffer[255], flag[16];
  unsigned row, col;
  e_bool_t start;
  int  dev_int, ret;

  if (!enif_get_string(env, argv[0], buffer, 255, ERL_NIF_LATIN1)) {
    if (enif_get_int(env, argv[0], &dev_int)){
      if (dev_int == 0){
	executable = NULL;
      } else {
	return enif_make_badarg(env);
      }
    } else {
	return enif_make_badarg(env);
    }
  } else {
    executable = buffer;
  }
  if (!enif_is_binary(env, argv[1])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[1], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[2], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[3], &col)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_atom(env, argv[4], flag, 16, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  } else {
    if (strncmp(flag, "true", 4) == 0) {
      start = E_TRUE;
    } else {
      start = E_FALSE;
    }
  }

  ret = e_load(executable, &dev, row, col, start);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_load_group function.
  int e_load_group(char *executable, e_epiphany_t *dev, unsigned row,
  unsigned col, unsigned rows, unsigned cols, e_bool_t start);
  Validate all inputs before function call.
  Return Value:
  ok | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_load_group_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  char *executable, buffer[255], flag[16];
  unsigned row, col, rows, cols;
  e_bool_t start;
  int  exe_int, ret;

  if (!enif_get_string(env, argv[0], buffer, 255, ERL_NIF_LATIN1)) {
    if (enif_get_int(env, argv[0], &exe_int)){
      if (exe_int == 0){
	executable = NULL;
      } else {
	return enif_make_badarg(env);
      }
    } else {
	return enif_make_badarg(env);
    }
  } else {
    executable = buffer;
  }
  if (!enif_is_binary(env, argv[1])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[1], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[2], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[3], &col)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[4], &rows)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[5], &cols)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_atom(env, argv[6], flag, 16, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  } else {
    if (strncmp(flag, "true", 4) == 0) {
      start = E_TRUE;
    } else {
      start = E_FALSE;
    }
  }

  ret = e_load_group(executable, &dev, row, col, rows, cols, start);

  switch(ret) {
  case E_OK:
    return(enif_make_atom(env, "ok"));
    break;
  default:
    return(enif_make_atom(env, "error"));
    break;
  }
}


/*********************************************************************
  Chapter 14.7 Utility Functions
  e_get_num_from_coords, e_get_coords_from_num, e_is_addr_on_chip,
  e_is_addr_on_group, e_set_host_verbosity, e_set_loader_verbosity
*********************************************************************/


/*--------------------------------------------------------------------
  Epiphany SDK e_get_num_from_coord function.
  unsigned e_get_num_from_coords(e_epiphany_t *dev, unsigned row,
  unsigned col);
  Validate all inputs before function call.
  Return Value:
  {ok, Coord} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_get_num_from_coords_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col, num;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &row)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[2], &col)) {
    return enif_make_badarg(env);
  }

  num = e_get_num_from_coords(&dev, row, col);

  return(enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, num)
			 )
	 );
}


/*--------------------------------------------------------------------
  Epiphany SDK e_get_coords_num_from_num function.
  void e_get_coords_from_num(e_epiphany_t *dev, unsigned corenum,
  unsigned *row, unsigned *col);
  Validate all inputs before function call.
  Return Value:
  {ok, {Row, Col}} | error

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_get_coords_from_num_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned row, col, num;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_int(env, argv[1], &num)) {
    return enif_make_badarg(env);
  }

  e_get_coords_from_num(&dev, num, &row, &col);

  return(enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_tuple(env, 2,
					 enif_make_int(env, row),
					 enif_make_int(env, col)
					 )
			 )
	 );
}


/*--------------------------------------------------------------------
  Epiphany SDK e_is_addr_on_chip function.
  e_bool_t e_is_addr_on_chip(void *addr);
  Validate all inputs before function call.
  Return Value:
  true | false

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_is_addr_on_chip_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  unsigned long addr;
  e_bool_t flag;

  if (!enif_get_ulong(env, argv[0], &addr)) {
    return enif_make_badarg(env);
  }

  flag = e_is_addr_on_chip((void *)addr);

  switch(flag) {
  case E_TRUE:
    return(enif_make_atom(env, "true"));
    break;
  default:
    return(enif_make_atom(env, "false"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_is_addr_on_group function.
  e_bool_t e_is_addr_on_group(e_epiphany_t *dev, void *addr);
  Validate all inputs before function call.
  Return Value:
  true | false

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_is_addr_on_group_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_epiphany_t dev;
  ErlNifBinary dev_bin;
  unsigned long addr;
  e_bool_t flag;

  if (!enif_is_binary(env, argv[0])) {
    return enif_make_badarg(env);
  } else {
    if (enif_inspect_binary(env, argv[0], &dev_bin)) {
      if ((dev_bin.size==sizeof(dev)) && (dev_bin.data != 0)) {
	memcpy(&dev, dev_bin.data, dev_bin.size);
      } else {
	enif_make_badarg(env);
      }
    } else {
      enif_make_badarg(env);
    }
  }
  if (!enif_get_ulong(env, argv[1], &addr)) {
    return enif_make_badarg(env);
  }

  flag = e_is_addr_on_group(&dev, (void *)addr);

  switch(flag) {
  case E_TRUE:
    return(enif_make_atom(env, "true"));
    break;
  default:
    return(enif_make_atom(env, "false"));
    break;
  }
}


/*--------------------------------------------------------------------
  Epiphany SDK e_set_host_verbosity function.
  e_hal_diag_t e_set_host_verbosity(e_hal_diag_t verbose);
  Validate all inputs before function call.
  Return Value:
  {ok, PreviousVerbosityLevel}

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_set_host_verbosity_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_hal_diag_t verbose, previous;

  if (!enif_get_int(env, argv[0], (int *)&verbose)) {
    return enif_make_badarg(env);
  }

  previous = e_set_host_verbosity(verbose);

  return(enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, previous)
			 )
	 );
}


/*--------------------------------------------------------------------
  Epiphany SDK e_set_loader_verbosity function.
  e_loader_diag_t e_set_loader_verbosity(e_hal_diag_t verbose);
  Validate all inputs before function call.
  Return Value:
  {ok, PreviousVerbosityLevel}

  See Epiphany SDK Reference for details.
*/
static ERL_NIF_TERM e_set_loader_verbosity_nif(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
  e_loader_diag_t verbose, previous;

  if (!enif_get_int(env, argv[0], (int *)&verbose)) {
    return enif_make_badarg(env);
  }
  
  previous = e_set_loader_verbosity(verbose);
  
  return(enif_make_tuple(env, 2,
			 enif_make_atom(env, "ok"),
			 enif_make_int(env, previous)
			 )
	 );
}


  

/*********************************************************************
  Erlang NIF (Native Implemented Functions) Library Setup
  The first tuple value is the function name as known to Erlang, the
  second value is the number of arguments and the third is the C
  function to call.
*********************************************************************/
static ErlNifFunc nif_funcs[] = {
  {"e_init", 1, e_init_nif},
  {"e_get_platform_info", 0, e_get_platform_info_nif},
  {"e_finalize", 0, e_finalize_nif},
  {"e_open", 4, e_open_nif},
  {"e_close", 1, e_close_nif},
  {"e_alloc", 2, e_alloc_nif},
  {"e_free", 1, e_free_nif},
  {"e_read", 5, e_read_nif},
  {"e_write", 6, e_write_nif},
  {"e_reset_system", 0, e_reset_system_nif},
  {"e_reset_group", 1, e_reset_group_nif},
  {"e_start", 3, e_start_nif},
  {"e_start_group", 1, e_start_group_nif},
  {"e_signal", 3, e_signal_nif},
  {"e_halt", 3, e_halt_nif},
  {"e_resume", 3, e_resume_nif},
  {"e_load", 5, e_load_nif},
  {"e_load_group", 7, e_load_group_nif},
  {"e_get_num_from_coords", 3, e_get_num_from_coords_nif},
  {"e_get_coords_from_num", 2, e_get_coords_from_num_nif},
  {"e_is_addr_on_chip", 1, e_is_addr_on_chip_nif},
  {"e_is_addr_on_group", 2, e_is_addr_on_group_nif},
  {"e_set_host_verbosity", 1, e_set_host_verbosity_nif},
  {"e_set_loader_verbosity", 1, e_set_loader_verbosity_nif}
};

ERL_NIF_INIT(ehal, nif_funcs, NULL, NULL, NULL, NULL)
