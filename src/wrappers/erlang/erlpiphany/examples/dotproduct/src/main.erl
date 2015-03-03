%%%-------------------------------------------------------------------
%%% @author linaro <linaro@linaro-nano>
%%% @copyright (C) 2015, linaro
%%% @doc
%%% This program is based on the dotproduct application that is part
%%% of the epiphany-examples distribution (contributed by Andreas
%%% Olofsson).
%%% The program follows as closely as possible the original C source
%%% in main.c
%%% 
%%% One thing of interest to note: numbers in Epiphany and ARM are in
%%% different endian forms
%%% so binaries of 32-bit data need to adjust the representation of
%%% the data read or written to memory accordingly.
%%% @end
%%% Created : 27 Jan 2015 by linaro <linaro@linaro-nano>
%%%-------------------------------------------------------------------
-module(main).

-export([main/0]).

-define(N,     4096).
-define(CORES, 16).

main() ->
    %% Calculation being done
    io:format("Calculating sum of products of two integer vectors of length ~p"
	      " initalized to all 0x1's using ~p Cores.~n", [?N, ?CORES]),
    io:format("........~n", []),
    %% Initialize the platform
    ok = ehal:e_init(0),
    ok = ehal:e_reset_system(),
    {ok, Platform} = ehal:e_get_platform_info(),
    {rows, Rows} = lists:keyfind(rows, 1, Platform),
    {cols, Cols} = lists:keyfind(cols, 1, Platform),
    {ok, Dev} = ehal:e_open(0, 0, Rows, Cols),
    %% Copy data from host to Epiphany local memory
    Bin = list_to_binary(lists:map(fun(_I) ->
    					   <<1:32/little>> % 32-bit "1"
    				   end, lists:seq(1, ?N div ?CORES))),
    Size_bin = size(Bin),
    Clr = <<0:32/little>>, % 32-bit "0"
    Size_clr = size(Clr),
    lists:map(fun(Core) ->
		      R = Core div 4,
		      C = Core rem 4,
		      {ok, Size_bin} =
			  ehal:e_write(Dev, R, C, 16#2000, Bin, Size_bin),
		      {ok, Size_bin} =
			  ehal:e_write(Dev, R, C, 16#4000, Bin, Size_bin),
		      {ok, Size_clr} =
			  ehal:e_write(Dev, R, C, 16#7000, Clr, Size_clr)
	      end, lists:seq(0, ?CORES-1)),
    %% Load program to cores and run
    ehal:e_load_group("e_task.srec", Dev, 0, 0, Rows, Cols, true),
    %%ehal:e_start_group(Dev),
    %% Check if all cores are done
    timer:sleep(10),
    check_done(Dev, 0),
    %% Retrieve results
    Results = lists:map(fun(Core) ->
				R = Core div 4,
				C = Core rem 4,
				{ok, {4, Data}} =
				    ehal:e_read(Dev, R, C, 16#6000, 4),
				[A1, A2, A3, A4] = binary_to_list(Data),
				((256*A4 + A3)*256 + A2)*256 + A1
			end, lists:seq(0, ?CORES-1)),
    %% Calculate final sum of products
    Sop = lists:foldl(fun(Prod, Acc) ->
			      Acc+Prod
		      end, 0, Results),
    %% Print out result
    io:format("Sum of Product Is ~w!~n", [Sop]),
    %% io:format("From ~p~n", [Results]),
    %% Close down Epiphany device
    ok = ehal:e_close(Dev),
    ok = ehal:e_finalize().


check_done(Dev, 0) ->
    Done = lists:foldl(fun(Core, Acc) ->
			       R = Core div 4,
			       C = Core rem 4,
			       {ok, {4, Data}} =
				   ehal:e_read(Dev, R, C, 16#7000, 4),
			       [A1, A2, A3, A4] = binary_to_list(Data),
			       Acc * (A1+A2+A3+A4)
		       end, 1, lists:seq(0, ?CORES-1)),
    check_done(Dev, Done);
check_done(_Dev, T) ->
    T.

