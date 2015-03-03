%%%-------------------------------------------------------------------
%%% @author Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015, Erlang Public License
%%% @doc
%%% This module duplicates the functionality of the Adapteva, Inc.
%%% C program of the same name, by Yaniv Sapir.
%%% 
%%% To quote the original program documentation
%%% 
%%% ```// This is the HOST side of the Hello World example.
%%% // The program initializes the Epiphany system,
%%% // randomly draws an eCore and then loads and launches
%%% // the device program on that eCore. It then reads the
%%% // shared external memory buffer for the core's output
%%% // message.'''
%%% 
%%% Make sure the erlpiphany include, ebin and priv directories are in
%%% your path or use/modify the init function below.
%%% @end
%%% Created : 21 Jan 2015 by Mark A Fleming <mark_fleming@ieee.org>
%%%-------------------------------------------------------------------
-module(hello_world).

%%-on_load(init/0).

-export([main/0, init/0]).

-define(BufSize, (128)).
-define(BufOffset, (16#01000000)).
-define(SeqLen, 20).


init() ->
    %% Add (relative) path to appropriate directories
    code:add_pathz("../../../include"),
    code:add_pathz("../../../priv"),
    code:add_pathz("../../../ebin").

main() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    
    %% initialize system
    ok = ehal:e_init(0),
    ok = ehal:e_reset_system(),
    {ok, Platform} = ehal:e_get_platform_info(),

    %% Allocate a buffer in shared memory
    {ok, Emem} = ehal:e_alloc(?BufOffset, ?BufSize),

    %% Process, recurse not loop
    loop(Platform, Emem, 0, ?SeqLen-1),

    %% Release buffer and finalize
    ok = ehal:e_free(Emem),
    ok = ehal:e_finalize().



loop(Plat, Emem, N, N) ->
    process(Plat, Emem, N);
loop(Plat, Emem, I, N) ->
    process(Plat, Emem, I),
    loop(Plat, Emem, I+1, N).



process(Plat, Emem, I) ->
    %% Platform info is a list of key-value tuples where key is the
    %% structure member name
    {row, Row} = lists:keyfind(row, 1, Plat),
    {col, Col} = lists:keyfind(col, 1, Plat),
    {rows, Rows} = lists:keyfind(rows, 1, Plat),
    {cols, Cols} = lists:keyfind(cols, 1, Plat),

    %% Draw a random core
    R = random:uniform(Rows)-1,
    C = random:uniform(Cols)-1,
    Coreid = (R+Row)*64 + C + Col,
    io:format("~3w: Message from eCore ~.16x (~2w,~2w): ", [I, Coreid, "0x", R, C]),

    %% Open single-core workgroup
    {ok, Dev} = ehal:e_open(R, C, 1, 1),
    ok = ehal:e_reset_group(Dev),

    %% Load program to selected core
    ok = ehal:e_load("e_hello_world.srec", Dev, 0, 0, true),

    %% Wait for program to finish
    timer:sleep(10),
    {ok, {?BufSize, Data}} = ehal:e_read(Emem, 0, 0, 0, ?BufSize),

    %% Print message, close workgroup
    Msg = binary_to_list(Data),
    String = lists:filter(fun(E) ->
				     E>0
			     end, Msg),
    io:format("~p~n", [String]),
    ok = ehal:e_close(Dev).
