-module(tke).
-behaviour(application).

-export([start/0, start/2, stop/1]).

%% Argument should be given on the command line like this:
%% erl -run tke start -extra Arguments
start() ->
    Arguments = parse_args(init:get_plain_arguments(), []),
    log:debug("Arguments=~p", [Arguments]),
    start_yaws(Arguments),
    %% List of projects to serve
    Projects = proplists:get_value(projects, Arguments),
    application:set_env(tke, projects, Projects),
    X = application:start(tke),
    log:debug("application:start(tke): ~p", [X]),
    X.

start(_Type, _Args) ->
    io:format("starting aplication tke..."),
    Sup  = tke_sup:start_link(),
    log:debug("Sup=~p", [Sup]),
    Sup.

stop(_State) ->
    ok.

%% start_yaws(Arguments) -> ok | error
%%    Arguments = proplist()
%%
start_yaws(Arguments) ->
    DocRoot = proplists:get_value(docroot, Arguments),
    Port = proplists:get_value(port, Arguments),
    Logdir = proplists:get_value(logdir, Arguments),
    GL=[{trace,false},
        {flags,[{auth_log,false},{copy_errlog,false}]}],
    SL=[{port,Port},{appmods,[{"/", tke_appmod}]},
        {flags,[{access_log,false}]}],
    yaws:start_embedded(DocRoot, SL, GL).

parse_args([], Result) -> Result;
parse_args(["--docroot", Arg | Rest], Acc) ->
    parse_args(Rest, [{docroot, Arg} | Acc]);
parse_args(["--port", Arg | Rest], Acc) ->
    parse_args(Rest, [{port, list_to_integer(Arg)} | Acc]);
parse_args(["--log-dir", Arg | Rest], Acc) ->
    parse_args(Rest, [{logdir, Arg} | Acc]);
parse_args(["--project", Arg | Rest], Acc) ->
    parse_args(["-p", Arg | Rest], Acc); % redirect to -p
parse_args(["-p", Arg | Rest], Acc) ->
    case proplists:get_value(projects, Acc) of
        undefined -> New_acc = [{projects, [Arg]} | Acc];
        Previous_value ->
            New_acc0 = proplists:delete(projects, Acc),
            New_acc = [{projects, [Arg | Previous_value]} | New_acc0]
    end,
    parse_args(Rest, New_acc);

parse_args([Other | Rest], Acc) -> parse_args(Rest, Acc).
    
