%%%-------------------------------------------------------------------
%%% @author Mark A Fleming <mark_fleming@ieee.org>
%%% @copyright (C) 2015, Erlang Public License
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2015 by Mark A Fleming <mark_fleming@ieee.org>
%%%-------------------------------------------------------------------
-module(erlpiphany_app).
-behaviour(application).
-export([start/2, stop/1]).


start(_Type, StartArgs) ->
    ok.

stop(_State) ->
    ok.
