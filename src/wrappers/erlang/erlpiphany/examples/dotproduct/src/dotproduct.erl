%%%-------------------------------------------------------------------
%%% @author Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015, Erlang Public License
%%% @doc
%%% This program is based on the dotproduct application that is part
%%% of the epiphany-examples distribution (contributed by Andreas
%%% Olofsson).
%%% 
%%% This program demonstrates working with Erlang processes by
%%% spawning a separate process per core to perform core
%%% initialization, program load, and retrieval of results. When
%%% complete, each process returns its result to the main process
%%% which summarizes the results.
%%% 
%%% @todo
%%% This program will usually crash as written in main.c, but the single
%%% process version does not. The cause seems to be the spin-wait on the
%%% done flag by all processes while waiting for each core to complete.
%%% When the spin-wait is replaced by a timer:sleep/1 call, no crashes
%%% occur. I suspect this may be due to the DMA problem discussed in the
%%% forum thread "Re: DMA crash?"
%%% @end
%%% Created : 26 Jan 2015 by Mark A Fleming <mark_fleming@ieee.org>
%%%-------------------------------------------------------------------
-module(dotproduct).

-define(N,     4096).
-define(CORES, 16).

-export([start/0, eCore/2]).


-spec start() -> {ok, Result :: integer(), Corelist :: list()} |
		 error.
%%%-------------------------------------------------------------------
%%% @doc
%%% Spawn processes to compute dotproduct in parallel on Epiphany.
%%% Initialize the system, then spawn a process per core to load a
%%% program, provide data for computation, and return the result.
%%% Collect the replies from the processes and display final result.
%%% @end
%%%-------------------------------------------------------------------
start() ->
    ok = ehal:e_init(0),
    ok = ehal:e_reset_system(),
    {ok, _Platform} = ehal:e_get_platform_info(),
    %% Spawn a process for each core
    _Pidlist = [{Core, spawn(?MODULE, eCore, [self(), Core])} ||
		  Core <- lists:seq(0, ?CORES-1)],
    %% Wait for each process to respond with a result
    Results = lists:map(fun(_I) ->
				receive
				    Response ->
					Response
				after 1500 ->
					%% If we time out, a process died
					timeout
				end
			end, lists:seq(0, ?CORES-1)),
    Order = lists:map(fun(Rslt) ->
			      case Rslt of
				  {_Pid, Core, _R} ->
				      Core;
				  timeout ->
				      timeout
			      end
		      end, Results),
    %% io:format("Spawned ~p processes: ~p~n", [?CORES, _Pidlist]),
    %% io:format("Processes returned ~p in order ~w~n", [Results, Order]),
    io:format("Processes returned in order ~w~n", [Order]),
    %% Check for success & print result
    Sop = lists:foldl(fun(_, fail) ->
			      fail;
			 (Result, Acc) ->
			      case Result of
				  {_, _, error} ->
				      fail;
				  {_, _, Dotprod} ->
				      Acc + Dotprod;
				  timeout ->
				      fail
			      end
		      end, 0, Results),
    io:format("Sum of Product Is ~p!~n", [Sop]),
    ok = ehal:e_finalize(),
    case Sop of
	fail ->
	    error;
	Sop ->
	    ok
    end.



-spec eCore(Host, Core) -> {Pid :: pid(), Core :: integer(),
			    Result :: integer()} |
			   {Pid :: pid(), Core :: integer(), error} when
      Host :: pid(),
      Core :: integer().
%%%-------------------------------------------------------------------
%%% @doc
%%% Interact with an assigned core to run a program and return the
%%% results.
%%% The program performs a dot product of two integer vectors of
%%% length N. A data area in external memory is set aside for each
%%% core to signal completion and store the result of computation.
%%% Each process will send the result to its parent and exit.
%%% @end
%%%-------------------------------------------------------------------
eCore(Host, Core) ->
    R = Core div 4,
    C = Core rem 4,
    Little_endian = fun([A1, A2, A3, A4]) ->
    			    ((A4*256 + A3)*256 + A2)*256 + A1
    		    end,
    %% Inefficient way, assumes N is not a power of 2
    %% Efficient way, knowing N is a power of two and the array is all ones
    Bin = list_to_binary([<<1:32/little>> ||
			     _I <- lists:seq(1, ?N div ?CORES)]),
    Size_bin = size(Bin),
    Clr = <<0:32/little>>, % 32-bit "0"
    Size_clr = size(Clr),
    %% Open a workgroup containing only our core
    {ok, Dev} = ehal:e_open(R, C, 1, 1),
    %% Copy data (N/CORE points) from host to Epiphany local memory
    {ok, Size_bin} = ehal:e_write(Dev, 0, 0, 16#2000, Bin, Size_bin),
    {ok, Size_bin} = ehal:e_write(Dev, 0, 0, 16#4000, Bin, Size_bin),
    {ok, Size_clr} = ehal:e_write(Dev, 0, 0, 16#7000, Clr, Size_clr),
    %% Load program to core
    Loaded = ehal:e_load("e_task.srec", Dev, 0, 0, true),
    timer:sleep(1),
    case Loaded of
	ok ->
	    %% Read back results
	    %% io:format("Core ~p reading result~n", [Core]),
	    {ok, {4, Result}} = ehal:e_read(Dev, 0, 0, 16#6000, 4),
	    %% Send result back to host and exit
	    %% io:format("Core ~p returning ~w~n", [Core, Little_endian(binary_to_list(Result))]),
	    Host ! {self(), Core, Little_endian(binary_to_list(Result))};
	error ->
	    Host ! {self(), Core, error}
    end,
    %% Cleanup and exit
    ok = ehal:e_close(Dev).
