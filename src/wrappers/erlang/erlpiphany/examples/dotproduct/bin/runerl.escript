#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    code:add_path("../../../priv"),
    code:add_path("../../../ebin"),
    application:load(erlpiphany),
    io:format("First run the serial version of the dotproduct program~n", []),
    case main:main() of
	ok ->
	    io:format("PASSED~n", []);
	error ->
	    io:format("FAILED~n", [])
    end,
    io:format("~nNow run the parallel process/eCore version~n", []),
    case dotproduct:start() of
	ok ->
	    io:format("PASSED~n", []);
	error ->
	    io:format("FAILED~n", [])
    end,
  ok.
