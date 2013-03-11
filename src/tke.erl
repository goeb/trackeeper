-module(tke).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    tke_sup:start_link().

stop(_State) ->
    ok.
