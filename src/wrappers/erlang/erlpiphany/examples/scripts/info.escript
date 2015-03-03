#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    %code:add_path("/home/linaro/Work/erlpiphany"),
    %code:add_path("/home/linaro/Work/erlpiphany/ebin"),
    %code:add_path("/home/linaro/Work/erlpiphany/priv"),
    code:add_path("../../"),
    code:add_path("../../ebin"),
    code:add_path("../../priv/"),
    application:load(erlpiphany),
    ok = ehal:e_init(0),
    ok = ehal:e_reset_system(),
    {ok, Platform} = ehal:e_get_platform_info(),
    io:format("Platform info: ~p~n", [Platform]),
    ok = ehal:e_finalize().
