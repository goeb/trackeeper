#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname tke verbose

main(["daemon"]) ->
    true = code:add_pathz(filename:dirname(escript:script_name()) 
                       ++ "/../ebin"),
    tke:start(),
    ok;

main(_) ->
    tke:start(),
    usage().

usage() ->
    io:format("Usage: tke <command>\n"),
    io:format("\n"),
    io:format("\tinit\n"),
    io:format("\tdaemon\n"),
    io:format("\tui\n"),
    halt(1).



