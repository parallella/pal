%%%-------------------------------------------------------------------
%%% @author  Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015, Erlang Public License
%%% @version 1.0.1
%%% @doc
%%% The Erlang interface to the Epiphany eHAL NIF loadable library.
%%% If module is not in the current working directory, use
%%% <br/>code:add_path("path to module directory").
%%% <br/>to include the module directory in the Erlang node search path.
%%% 
%%% This module provides the Erlang interface to the Epiphany SDK eHAL
%%% function set. Every effort has been made to hew to the name and
%%% argument list order of the original C functions. This was done to
%%% simplify the porting of C code to Erlang, to maintain familiarity as
%%% much as possible for the experienced SDK user, and to avoid the usual
%%% temptation to "improve" the original source arrangement. (Why, oh why,
%%% does the e_load argument list start off with a pointer to the program
%%% executable instead of the device structure like all other functions?)
%%% 
%%% All SDK C structure values are represented as Erlang binaries. This is
%%% done to isolate Erlang code from any changes to the SDK definitions.
%%% An Erlang program will just pass these binary blobs around, leaving it up
%%% to the NIF functions to transform them to amd from C structures.
%%%
%%% Many of the SDK's defined values and enumerations can be found in the
%%% Erlang 'ehal.hrl' include file. The file defines macros to represent
%%% such constants as E_REG_CONFIG (use ?E_REG_CONFIG).
%%% A small deviation from the SDK standard was to use the boolean atoms
%%% 'true', 'false', 'ok' and 'error' in place of the defined integer values
%%% E_TRUE, E_FALSE, E_OK and E_ERR respectively.
%%% 
%%% Module functions use the Erlang convention of returning the atoms
%%% 'ok' and 'error' or the tuple {ok, Result}. Most of the C functions
%%% return a success flag and use pointer arguments to return other values.
%%% In Erlang, the e_open() function returns {ok, Dev} for the e_epiphany_t
%%% structure returned by the corresponding C function.
%%% 
%%% 
%%% @end
%%% Created : 12 Jan 2015 by  Mark A Fleming
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



-type platform_info() :: [ platform_data() ].
-type platform_data() :: {row,       integer()} |
			 {col,       integer()} |
			 {version,   string()}  |
			 {num_chips, integer()} |
			 {num_emems, integer()}.
-type epiphany_dev()  :: binary().
-type epiphany_mem()  :: binary().


init() ->
    ok = erlang:load_nif("./ehal", 0).



-spec e_init(HDF) -> ok | error when
      HDF :: string() | 0.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function initializes the HAL data structures, and establishes
%%% a connection to the Epiphany platform.
%%% The platform parameters are read form a Hardware Description File
%%% (HDF), whose path is given as the function argument.
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



-spec e_get_platform_info() -> {ok, platform_info()} | error.
%%%-------------------------------------------------------------------
%%% @doc
%%% This function returns information about the Epiphany platform as
%%% a list of tuples of the form {attribute, Value}.
%%% Valid attributes at this time are
%%% <br/> row, col, version, num_chips, num_emems
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
%%% This function finalizes the connection to the Epiphany and releases
%%% resources allocated in the e_init function.
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



-spec e_open(Row, Col, Rows, Cols) -> {ok, epiphany_dev()} | error when
      Row  :: integer(),
      Col  :: integer(),
      Rows :: integer(),
      Cols :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function defines a eCore workgroup. The (Row, Col) defines
%%% the workgroup origin relative to the platform origin and (Rows, Cols)
%%% gives the workgroup size. The function returns a binary that can
%%% be used in subsequent calls to reference the workgroup.
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
%%% allocated in the e_open function.
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



-spec e_alloc(Base, Size) -> {ok, epiphany_mem()} | error when
      Base :: integer(),
      Size :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% This function allocates a buffer in external memory of size 'Size'
%%% bytes starting at offset 'Base' bytes from the beginning of external
%%% memory. If successful, a reference is returned that can be used in
%%% subsequent read and write operations.
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
%%% This function releases the external memory buffer allocated by the
%%% e_alloc() function.
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
%%% This function reads data from a workgroup core local memory if Dev
%%% is of type e_epiphany_t or from an external memory buffer if Dev
%%% is of type e_mem_t. If successful, a tuple containing the number
%%% of bytes read and a binary of the data read is returned.
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
%%% is of type e_epiphany_t or to an external memory buffer if Dev
%%% is of type e_mem_t. If successful, the number of bytes written
%%% is returned.
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
%%% platform.
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
%%% This function performs a soft reset of a eCore workgroup.
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
%%% loaded. The (Row, Col) eCore coordinate is relative to the
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
%%% The (Row, Col) eCore coordinate is relative to the
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
%%% The (Row, Col) eCore coordinate is relative to the
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
%%% was previously halted. The (Row, Col) eCore coordinate is relative
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
%%% This function loads a program into a workgroup's core. The Program
%%% string points to a file in SREC format. The (Row, Col) address of
%%% the core is relative to the workgroup's origin given as Dev. When
%%% the Start value is true, the program is launched after it is loaded.
%%%
%%% Program load should only be performed when a core is in an idle or
%%% halt state, ideally after a e_reset_system() or e_reset_core().
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
%%% The (Row, Col) address of the core group is relative to the
%%% workgroup's origin given as Dev. The range of cores in the subgroup
%%% is given by Rows and Cols.
%%% When the Start value is true, the program is launched after it is
%%% loaded.
%%%
%%% Program load should only be performed when a core is in an idle or
%%% halt state, ideally after a e_reset_system() or e_reset_core().
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
%%% This function returns true if the given 32-bit address is within
%%% the Epiphany's physical address space.
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
%%% This function returns true if the given 32-bit address is within
%%% the workgroup cores physical address space.
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
%%% Diagnostic levels are defined by the macros H_D0 to H_D4 in the
%%% include file ehal.hrl
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
%%% Diagnostic levels are defined by the macros L_D0 to L_D4 in the
%%% include file ehal.hrl
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
