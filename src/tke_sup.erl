-module(tke_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_new_project/1]).

start_link() ->
    supervisor:start_link(tke_sup, []).

init(_Args) ->
    case application:get_env(projects) of
        undefined ->
            log:debug("no project to load"),
            Childspecs = [];
        {ok, Projects} ->
            log:debug("Projects to load = ~p", [Projects]),
            Childspecs = make_childspecs_tke_db(Projects, [])
    end,
    {ok, {{one_for_one, 1, 5}, Childspecs}}.

%% Prepare the children specifications for starting
%% the instances of tke_db for each project.
make_childspecs_tke_db([], Acc) -> Acc;
make_childspecs_tke_db([P | Other], Acc) ->
    New_acc = [{tke_db, {tke_db, start, [P]},
        permanent, 10000, worker, [tke_db]} | Acc],
    make_childspecs_tke_db(Other, New_acc).


%% Start a project at runtime
start_new_project(Project_path) ->
    ChildSpec = make_childspecs_tke_db([Project_path], []),
    log:debug("start_new_project: ChildSpec=~p", [ChildSpec]),
    X = supervisor:start_child(tke_sup, ChildSpec),
    log:debug("start_new_project: start_child=~p", [X]),
    X.
