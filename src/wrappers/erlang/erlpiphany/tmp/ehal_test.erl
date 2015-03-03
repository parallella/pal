%%% @author  Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015, Erlang Public License
%%% @doc
%%% Test routines for the ehal NIF library.
%%% @end
%%% Created : 14 Jan 2015 by Mark A Fleming

-module(ehal_test).

-export([stub_test/0, open_test/0, xmem_test/0, imem_test/0, load_test/0,
	 util_test/0, diag_test/0]).

-include("ehal.hrl").


stub_test() ->
    io:format("Hello, World!~n", []).

open_test() ->
    %% Test the init with a proper configuration file, reset,
    %% get platform information, open a workgroup and close it
    ok = ehal:e_init("/opt/adapteva/esdk/bsps/current/platform.hdf"),
    ehal:e_reset_system(),
    R = ehal:e_get_platform_info(),
    io:format("Platform info = ~p~n", [R]),
    {ok, Dev} = ehal:e_open(0, 0, 2, 2),
    %% io:format("Got device structure ~p~n", [Dev]),
    ok = ehal:e_close(Dev),
    ok = ehal:e_finalize().


xmem_test() ->
    %%code:ensure_loaded(list_to_atom("./ehal.beam")),
    %% Test the init with a NULL configuration file argument, allocate
    %% an external memory buffer, read and write to the buffer, free buffer
    ok = ehal:e_init(0),
    ehal:e_reset_system(),
    R = ehal:e_get_platform_info(),
    io:format("Platform info = ~p~n", [R]),
    {ok, Mem} = ehal:e_alloc(16#01000000, 64),
    %% io:format("Got e_mem_t structure ~p~n", [Mem]),
    String = "Hello, Parallella!",
    Len = length(String),
    Bin = list_to_binary(String),
    {ok, Wsize} = ehal:e_write(Mem, 0, 0, 0, Bin, Len),
    %% io:format("Wrote ~p bytes for data ~p~nString=~p~n",
    %% 	      [_Wsize, Bin, binary_to_list(Bin)]),
    {ok, {Size, Data}} = ehal:e_read(Mem, 0, 0, 0, Len),
    %% io:format("Read ~p bytes from external memory: ~p~nString=~p~n",
    %% 	      [_Size, Data, binary_to_list(Data)]),
    ok = ehal:e_free(Mem),
    if
	Data==Bin,
	Wsize==Len,
	Size==Len ->
	    ok;
	true ->
	    fail
    end.


ecore_mem([Core|Corelist], Result) ->
    %% Open a workgroup with a single core,
    %% Allocate a buffer in an eCore memory, read/write then free buffer
    %% Close workgroup
    ok = ehal:e_init(0),
    ehal:e_reset_system(),
    Row = Core div 4,
    Col = Core rem 4,
    {ok, Dev} = ehal:e_open(Row, Col, 1, 1),
    %% io:format("Got e_epiphany_t structure ~p~n", [Dev]),
    Bin = list_to_binary("Hello, Parallella!"),
    {ok, Wsize} = ehal:e_write(Dev, 0, 0, 16#1000, Bin, 18),
    {ok, {Size, Data}} = ehal:e_read(Dev, 0, 0, 16#1000, 18),
    ok = ehal:e_close(Dev),
    ok = ehal:e_finalize(),
    if
	Data==Bin, Wsize==18, Size==18 ->
	    Rslt = {ok, Core};
	true ->
	    Rslt = {fail, Core}
    end,
    ecore_mem(Corelist, [Rslt|Result]);
ecore_mem([], Result) ->
    Result.


imem_test() ->
    %% Allocate a buffer in each of the 16 eCores and read/write memory
    Cores = lists:seq(0, 15),
    Result = ecore_mem(Cores, []),
    io:format("Result of testing core memories is ~p~n", [Result]).


load_test() ->
    %% Test the program load functions
    ok = ehal:e_init(0),
    ehal:e_reset_system(),
    {ok, Dev} = ehal:e_open(0, 0, 3, 3),
    ok = ehal:e_reset_group(Dev),
    ok = ehal:e_load("./e_hello_world.srec", Dev, 0, 0, false),
    ok = ehal:e_load_group("./e_hello_world.srec", Dev, 0, 0, 2, 2, false),
    %% This blows up because the eHal lib doesn't check workgroup bounds
    %%error = ehal:e_load_group("./e_hello_world.srec", Dev, 0, 0, 4, 4, false),
    ok = ehal:e_close(Dev),
    ok = ehal:e_finalize().


ecore_util([Core|Corelist], Result) ->
    R = Core div 4,
    C = Core rem 4,
    ok = ehal:e_init(0),
    ehal:e_reset_system(),
    %% Get base address of eCore memory
    {ok, Plat} = ehal:e_get_platform_info(),
    {row,  ChipRow} = lists:keyfind(row, 1, Plat),
    {col,  ChipCol} = lists:keyfind(col, 1, Plat),
    Off = 16#80000,
    Base = ((ChipRow+R)*64 + ChipCol + C)*Off,
    {ok, Dev} = ehal:e_open(0, 0, 4, 4),
    {ok, {Row, Col}} = ehal:e_get_coords_from_num(Dev, Core),
    {ok, Coord} = ehal:e_get_num_from_coords(Dev, R, C),
    ChipT = ehal:e_is_addr_on_chip(Base),
    GroupT = ehal:e_is_addr_on_group(Dev, Base),
    GroupF = ehal:e_is_addr_on_group(Dev, Base + 17*Off),
    if Row==R,
       Col==C,
       Coord==Core,
       ChipT == true,
       GroupT == true,
       GroupF == false ->
	    Rslt = {ok, Core};
       true ->
	    Rslt = {fail, Core, Row, Col, Coord, ChipT, GroupT, GroupF}
    end,
    ecore_util(Corelist, [Rslt|Result]);
ecore_util([], Result) ->
    Result.

util_test() ->
    %% Test the coordinate and address utility functions
    Cores = lists:seq(0, 15),
    Result = ecore_util(Cores, []),
    io:format("Result of testing core utilities is ~p~n", [Result]).


diag_test() ->
    %% Test the diagnostic level utility functions
    ok = ehal:e_init(0),
    ehal:e_reset_system(),
    {ok, DefaultHal} = ehal:e_set_host_verbosity(?H_D0),
    {ok, ?H_D0} = ehal:e_set_host_verbosity(?H_D1),
    {ok, ?H_D1} = ehal:e_set_host_verbosity(?H_D2),
    {ok, ?H_D2} = ehal:e_set_host_verbosity(?H_D3),
    {ok, ?H_D3} = ehal:e_set_host_verbosity(?H_D4),
    {ok, ?H_D4} = ehal:e_set_host_verbosity(DefaultHal),
    {ok, DefaultLdr} = ehal:e_set_host_verbosity(?L_D0),
    {ok, ?L_D0} = ehal:e_set_host_verbosity(?L_D1),
    {ok, ?L_D1} = ehal:e_set_host_verbosity(?L_D2),
    {ok, ?L_D2} = ehal:e_set_host_verbosity(?L_D3),
    {ok, ?L_D3} = ehal:e_set_host_verbosity(?L_D4),
    {ok, ?L_D4} = ehal:e_set_host_verbosity(DefaultLdr),
    {ok, DefaultHal, DefaultLdr}.
