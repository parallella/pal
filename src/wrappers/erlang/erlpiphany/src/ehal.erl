%%%-------------------------------------------------------------------
%%% @author  Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015
%%% @version 1.0.1
%%% @doc
%%% This is the Erlang interface to the Epiphany eHAL NIF loadable library.
%%% If the module is not in the current working directory, use
%%% ```code:add_path("path to module directory")'''
%%% to include the module directory in the Erlang node search path.
%%% 
%%% == Introduction ==
%%% This module provides an Erlang interface to the Epiphany SDK eHAL
%%% function set. Every effort has been made to hew to the name and
%%% argument list order of the original C functions. This was done to
%%% simplify the porting of C code to Erlang, to maintain familiarity as
%%% much as possible for the experienced SDK user, and to avoid the usual
%%% temptation to "improve" the original source arrangement.
%%% 
%%% == Erlang Term Conventions ==
%%% All SDK C structure values are represented as Erlang binaries. This is
%%% done to isolate Erlang code from any changes to the SDK structure
%%% definitions.
%%% An Erlang program will just pass these binary blobs around, leaving it up
%%% to the NIF functions to transform them to and from the corresponding
%%% C structures.
%%% An experienced programmer can use the bit syntax to prize apart the
%%% binary into the corresponding structure members.
%%%
%%% Data written to or read from an eCore internal memory or Epiphany
%%% external memory is also represented as a binary. Erlang has a number
%%% of ways to convert between regular Erlang terms and binaries.
%%% For example, `list_to_binary("Hello World")' can be used to put a
%%% string in binary format and `binary_to_list(Bin)' can do the reverse.
%%% More detail can be found in `erlang(3)'
%%%
%%% Many of the SDK's defined values and enumerations can be found in the
%%% Erlang 'ehal.hrl' include file. The file defines macros to represent
%%% such constants as E_REG_CONFIG (use ?E_REG_CONFIG).
%%% A small deviation from the SDK standard was to use the boolean atoms
%%% 'true', 'false', 'ok' and 'error' in place of the defined integer values
%%% E_TRUE, E_FALSE, E_OK and E_ERR respectively.
%%% 
%%% Module functions use the Erlang convention of returning the atoms
%%% 'ok' and 'error' or the tuple '{ok, Result}'. For example, most of
%%% the SDK C functions return a success flag and use pointer
%%% arguments to return other values.
%%% In Erlang, the 'e_open/4' function returns '{ok, Dev}' for the
%%% `e_epiphany_t' structure returned by the corresponding C function.
%%% 
%%% 
%%% @end
%%% Created : 12 Jan 2015 by  Mark A Fleming

%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met: 

%% 1. Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.

%% 2. Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.

%% 3. Neither the name of the <ORGANIZATION> nor the names of its
%% contributors may be used to endorse or promote products derived from
%% this software without specific prior written permission.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(ehal).

-export([init/0,
	 e_init/1, e_get_platform_info/0, e_finalize/0,
	 e_open/4, e_close/1, e_alloc/2, e_free/1,
	 e_read/5, e_write/6,
	 e_reset_system/0, e_reset_group/1, e_start/3, e_start_group/1,
	 e_signal/3, e_halt/3, e_resume/3,
	 e_load/5, e_load_group/7,
	 e_get_num_from_coords/3, e_get_coords_from_num/2,
	 e_is_addr_on_chip/1, e_is_addr_on_group/2,
	 e_set_host_verbosity/1, e_set_loader_verbosity/1]).

-on_load(init/0).
-define(APPNAME, erlpiphany).



-type platform_info() :: [ platform_data() ].
-type platform_data() :: {row,       integer()} |
			 {col,       integer()} |
			 {rows,      integer()} |
			 {cols,      integer()} |
			 {version,   string()}  |
			 {regs_base, integer()} |
			 {num_chips, integer()} |
			 {num_emems, integer()}.
-type epiphany_dev()  :: binary().
-type epiphany_mem()  :: binary().



-spec init() -> ok | error.
%%%-------------------------------------------------------------------
%%% @doc
%%% Search for and load the NIF loadable library.
%%% This function is automatically called with the module is loaded.
%%% The library should be in the erlpiphany priv directory, which is
%%% searched first. If not found, check the default search paths for
%%% the library, and lastly, check the current working directory.
%%% @end
%%%-------------------------------------------------------------------
init() ->
    case code:priv_dir(?APPNAME) of
	{error, _Reason} ->
	    %% Check erlpiphany application priv directory
	    %% The application must be loaded via a call to
	    %% application:load(erlpiphany)
	    error_logger:format("~w priv dir not found~n", [?APPNAME]),
	    %% See if it is in one of the paths
	    case code:where_is_file("ehal.so") of
		non_existing ->
		    error_logger:format("ehal.so not in path~n", []),
		    %% Try current directory
		    case erlang:load_nif("./ehal", 0) of
			{error, _R} ->
			    {ok, Dir} = file:get_cwd(),
			    error_logger:format("ehal.so not in ~w~n",
						[Dir]),
			    %%exit(error);
			    error;
			ok ->
			    ok
		    end;
		SOfile ->
		    erlang:load_nif(filename:rootname(SOfile), 0)
	    end;
	PrivDir ->
	    erlang:load_nif(filename:join([PrivDir, "ehal"]), 0)
    end.



-spec e_init(HDF) -> ok | error when
      HDF :: string() | 0.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function initializes the HAL data structures, and establishes
%%% a connection to the Epiphany platform.
%%% The platform parameters are read from a Hardware Description File
%%% (HDF), whose path is given as the function argument.
%%% If the HDF argument is a 0 value or empty string, the function will
%%% use environment variable information (EPIPHANY_HDF or EPIPHANY_HOME)
%%% to try to locate the default HDF file.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_init(_HDF) ->
    exit(ehal_library_not_loaded).



-spec e_get_platform_info() -> {ok, Plat :: platform_info()} | error.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function returns information about the Epiphany platform as
%%% a list of tuples of the form {attribute, Value}.
%%% Valid attributes at this time are:
%%% <br/> row, col, rows, cols, version, num_chips, num_emems, regs_base
%%% 
%%% Use lists:keyfind/2 to extract a desired key-value pair.
%%%
%%% @todo The regs_base value is a 32-bit integer that can be negative
%%% if the high-order bit is set. The value should be returned as an
%%% unsigned integer.
%%%
%%% It would also be useful to return the chip and memory segment
%%% structures as sublists in the main attribute list.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_get_platform_info() ->
    exit(ehal_library_not_loaded).



-spec e_finalize() -> ok | error.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function finalizes the connection to the Epiphany chip and
%%% releases resources allocated by the e_init function.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_finalize() ->
    exit(ehal_library_not_loaded).



-spec e_open(Row, Col, Rows, Cols) -> {ok, Dev :: epiphany_dev()} |
				      error when
      Row  :: integer(),
      Col  :: integer(),
      Rows :: integer(),
      Cols :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function defines a eCore workgroup. The Row and Col
%%% parameters defines
%%% the workgroup origin relative to the platform origin and the Rows
%%% and Cols parameters gives the workgroup size.
%%% The function returns a binary (e_epiphany_t structure) that can
%%% be used in subsequent calls to reference the workgroup.
%%% The e_get_platform_info/0 function can be used to determine the
%%% size of the Epiphany chip in rows and columns.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_open(_Row, _Col, _Rows, _Cols) ->
    exit(ehal_library_not_loaded).



-spec e_close(Dev) -> ok | error when
      Dev :: epiphany_dev().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function closes the eCode workgroup and releases resources
%%% allocated by the e_open function. This function should be used
%%% before re-allocating an eCore member to a new workgroup.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_close(_Dev) ->
    exit(ehal_library_not_loaded).



-spec e_alloc(Base, Size) -> {ok, Mbuf :: epiphany_mem()} | error when
      Base :: integer(),
      Size :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function defines a buffer in external memory of size 'Size'
%%% bytes starting at offset 'Base' bytes from the beginning of external
%%% memory. If successful, a binary (e_mem_t structure) is returned
%%% that can be used in subsequent read and write operations.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_alloc(_Base, _Size) ->
    exit(ehal_library_not_loaded).



-spec e_free(Mbuf) -> ok | error when
      Mbuf :: epiphany_mem().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function releases the resources allocated by the
%%% e_alloc/2 function.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_free(_Mbuf) ->
    exit(ehal_library_not_loaded).



-spec e_read(Dev, Row, Col, From, Size) -> {ok, {Ssize :: integer(),
						 Data  :: binary()}} |
					   error when
      Dev  :: epiphany_dev() | epiphany_mem(),
      Row  :: integer(),
      Col  :: integer(),
      From :: integer(),
      Size :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function reads data from a workgroup eCore local memory if Dev
%%% is of type `e_epiphany_t' or from an external memory buffer if Dev
%%% is of type `e_mem_t'. If successful, a tuple containing the number
%%% of bytes read and a binary of the data read is returned. If
%%% accessing the core local registers, use one of the register symbol
%%% macros in ehal.hrl as the From offset.
%%%
%%% The From argument specifies an offset from the base of the eCore
%%% internal memory or from the external memory buffer. The Row and Col
%%% arguments specify a particular eCore when reading from local
%%% memory and are ignored when reading from external memory.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_read(_Dev, _Row, _Col, _From, _Size) ->
    exit(ehal_library_not_loaded).



-spec e_write(Dev, Row, Col, To, Buf, Size) -> {ok, Ssize :: integer()} |
					       error when
      Dev  :: epiphany_dev() | epiphany_mem(),
      Row  :: integer(),
      Col  :: integer(),
      To   :: integer(),
      Buf  :: binary(),
      Size :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function writes data to a workgroup core local memory if Dev
%%% is of type `e_epiphany_t' or to an external memory buffer if Dev
%%% is of type `e_mem_t'. If successful, the number of bytes written
%%% is returned. If accessing the core local registers, use one of the
%%% register symbol macros in ehal.hrl as the To offset.
%%%
%%% Data is passed to this function in the form of a binary. A binary
%%% can be created from common Erlang terms using such functions as
%%% atom_to_binary/2, float_to_binary/1, integer_to_binary/1
%%% or list_to_binary/1. Use size/1 to get the size of a binary.
%%% 
%%% Since any character sequence within single
%%% quotes is an Erlang atom, atom_to_binary/2 is one method of
%%% encoding strings, for example
%%% <br/> atom_to_binary('Hello, World!', latin1)
%%% <br/> See erlang(3) for more details and examples.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_write(_Dev, _Row, _Col, _To, _Buf, _Size) ->
    exit(ehal_library_not_loaded).



-spec e_reset_system() -> ok | error.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function performs a full hardware reset of the Epiphany
%%% platform, including the Epiphany chip and FPGA glue logic.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_reset_system() ->
    exit(ehal_library_not_loaded).



-spec e_reset_group(Dev) -> ok | error when
      Dev :: epiphany_dev().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function performs a soft reset of a eCore workgroup. See SDK
%%% documentation for cautions on using this function.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_reset_group(_Dev) ->
    exit(ehal_library_not_loaded).



-spec e_start(Dev, Row, Col) -> ok | error when
      Dev :: epiphany_dev(),
      Row :: integer(),
      Col :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function starts a core, normally after a program has been
%%% loaded. The Row and Col eCore coordinate is relative to the
%%% workgroup given as Dev.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_start(_Dev, _Row, _Col) ->
    exit(ehal_library_not_loaded).



-spec e_start_group(Dev) -> ok | error when
      Dev :: epiphany_dev().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function starts all cores in a workgroup, normally after a
%%% program has been loaded.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_start_group(_Dev) ->
    exit(ehal_library_not_loaded).



-spec e_signal(Dev, Row, Col) -> ok | error when
      Dev :: epiphany_dev(),
      Row :: integer(),
      Col :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function sends a soft interrupt to a workgroup core.
%%% The Row and Col eCore coordinate is relative to the
%%% workgroup given as Dev.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_signal(_Dev, _Row, _Col) ->
    exit(ehal_library_not_loaded).



-spec e_halt(Dev, Row, Col) -> ok | error when
      Dev :: epiphany_dev(),
      Row :: integer(),
      Col :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function halts a workgroup core's program execution.
%%% The Row and Col eCore coordinate is relative to the
%%% workgroup given as Dev.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_halt(_Dev, _Row, _Col) ->
    exit(ehal_library_not_loaded).



-spec e_resume(Dev, Row, Col) -> ok | error when
      Dev :: epiphany_dev(),
      Row :: integer(),
      Col :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function resumes a workgroup core's program execution that
%%% was previously halted with a call to e_halt.
%%% The Row and Col eCore coordinate is relative
%%% to the workgroup given as Dev.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_resume(_Dev, _Row, _Col) ->
    exit(ehal_library_not_loaded).



-spec e_load(Program, Dev, Row, Col, Start) -> ok | error when
      Program :: string(),
      Dev     :: epiphany_dev(),
      Row     :: integer(),
      Col     :: integer(),
      Start   :: true | false.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function loads a program into a workgroup core. The Program
%%% string points to a file in SREC format. The Row and Col address of
%%% the core is relative to the workgroup's origin. When
%%% the Start value is 'true', the program is launched after it is
%%% loaded. The function also accepts a null or zero Program parameter.
%%%
%%% Program load should only be performed when a core is in an idle or
%%% halt state, ideally after a e_reset_system/0 or e_reset_group/1.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_load(_Program, _Dev, _Row, _Col, _Start) ->
    exit(ehal_library_not_loaded).



-spec e_load_group(Program, Dev, Row, Col, Rows, Cols, Start) -> ok | error when
      Program :: string(),
      Dev     :: epiphany_dev(),
      Row     :: integer(),
      Col     :: integer(),
      Rows    :: integer(),
      Cols    :: integer(),
      Start   :: true | false.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function loads a program into a subgroup of a workgroup's
%%% cores. The Program string points to a file in SREC format.
%%% The Row and Col address of the core group is relative to the
%%% workgroup's origin. The range of cores in the subgroup
%%% is given by Rows and Cols.
%%% When the Start value is 'true', the program is launched after it is
%%% loaded.
%%%
%%% Program load should only be performed when a core is in an idle or
%%% halt state, ideally after a e_reset_system/0 or e_reset_group/1.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_load_group(_Program, _Dev, _Row, _Col, _Rows, _Cols, _Start) ->
    exit(ehal_library_not_loaded).



-spec e_get_num_from_coords(Dev, Row, Col) -> {ok, integer()} | error when
      Dev :: epiphany_dev(),
      Row :: integer(),
      Col :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function converts a workgroup's core coordinate to the core
%%% number within the workgroup.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_get_num_from_coords(_Dev, _Row, _Col) ->
    exit(ehal_library_not_loaded).



-spec e_get_coords_from_num(Dev, Corenum) -> {ok, {Row :: integer(),
						   Col :: integer()}} |
					     error when
      Dev     :: epiphany_dev(),
      Corenum :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function converts a workgroup's core number to the core
%%% coordinate relative to the workgroup origin.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_get_coords_from_num(_Dev, _Corenum) ->
    exit(ehal_library_not_loaded).



-spec e_is_addr_on_chip(Addr) -> true | false when
      Addr :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function returns `true' if the given 32-bit address is within
%%% the Epiphany's physical address space, or `false' otherwise.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_is_addr_on_chip(_Addr) ->
    exit(ehal_library_not_loaded).



-spec e_is_addr_on_group(Dev, Addr) -> true | false when
      Dev  :: epiphany_dev(),
      Addr :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function returns `true' if the given 32-bit address is within
%%% the workgroup cores physical address space, or `false' otherwise.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_is_addr_on_group(_Dev, _Addr) ->
    exit(ehal_library_not_loaded).



-spec e_set_host_verbosity(Level) -> integer() when
      Level :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function sets the verbosity level of eHAL function calls.
%%% The old diagnostic level is returned.
%%% Diagnostic levels are defined by the macros ?H_D0 to ?H_D4 in the
%%% 'ehal.hrl' include file. Level ?H_D0 means no diagnostics and each
%%% remaining level provides more detailed diagnostics.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_set_host_verbosity(_Level) ->
    exit(ehal_library_not_loaded).



-spec e_set_loader_verbosity(Level) -> integer() when
      Level :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function sets the verbosity level of program loader function
%%% calls. The old diagnostic level is returned.
%%% Diagnostic levels are defined by the macros ?L_D0 to ?L_D4 in the
%%% 'ehal.hrl' include file. Level ?L_D0 means no diagnostics and each
%%% remaining level provides more detailed diagnostics.
%%% ===See Also===
%%% <a href="http://www.adapteva.com/docs/epiphany_sdk_ref.pdf">
%%% Epiphany SDK Reference</a>
%%%
%%% <a href="http://www.adapteva.com/all-documents/">
%%% External Documentation Archive</a>
%%% @end
%%%-------------------------------------------------------------------
e_set_loader_verbosity(_Level) ->
    exit(ehal_library_not_loaded).
