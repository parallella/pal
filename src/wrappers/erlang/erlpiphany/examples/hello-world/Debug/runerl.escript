#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
  code:add_path("../../../priv"),
  code:add_path("../../../ebin"),
  code:add_path("../../.."),
  application:load(erlpiphany),
  hello_world:main().
