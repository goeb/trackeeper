%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Handle interactions with the database
%%%

-module(tke_base).

-compile(export_all).

% return the list of issues of the given project
list(Project_name) -> list(Project_name, []).

list(Project_name, Options) ->
    log:debug("list(~p, ~p)", [Project_name, Options]),
    Fd = sqlite3:open(db, [ {file, Project_name ++ "/db"}]),
    log:debug("sqlite3:open Fd=~p", [Fd]),
    case Fd of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    sqlite3:sql_exec(db, "select rowid, * from issue;").



