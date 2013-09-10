#!/usr/bin/env escript

main(_) ->
    os:cmd("cp deps/bullet/priv/bullet.js priv/js/"),
    os:cmd("cp deps/bullet_bert/priv/*.js priv/js/"), 
    [code:add_path(Dir) || Dir <- ["ebin" | filelib:wildcard("deps/*/ebin")]],
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(bullet_bert),
    ok = application:start(clock),
    receive _ -> ok end.
