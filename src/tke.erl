-module(tke).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start() -> application:start(tke).

start(_Type, _Args) ->
    Link = {ok, Sup} = tke_sup:start_link(),
    {ok, Children_specs} = start_yaws(),
    [supervisor:start_child(Sup, Ch) || Ch <- Children_specs],
    Link.

stop(_State) ->
    ok.

start_yaws() ->
    Docroot = ".",
    SconfList = [{docroot, Docroot},
                 {port, 8000},
                 {listen, {127,0,0,1}},
                 {appmods, [{"/", tke_appmod}]}],
    GconfList = [ebin_dir, [Docroot ++ "/ebin"]], % TODO ebin_dir
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList),
    log:debug("yaws started: ChildSpecs=~p", [ChildSpecs]),
    %TODO
    %TODO
    yaws_api:setconf(GC, SCList),
    {ok, ChildSpecs}.

