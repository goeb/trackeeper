-module(tke_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(tke_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 5},
          [{tke_db, {tke_db, start, ["tke"]},
            permanent, 10000, worker, [tke_db]}]}}.
