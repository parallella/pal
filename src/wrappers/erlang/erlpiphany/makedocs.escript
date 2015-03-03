#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    code:add_path("/home/linaro/Work/erlpiphany"),
    code:add_path("/home/linaro/Work/erlpiphany/ebin"),
    code:add_path("/home/linaro/Work/erlpiphany/priv"),
    edoc:application(erlpiphany, ".", []).
