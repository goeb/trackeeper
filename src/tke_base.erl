-module(tke_base).

-compile(export_all).

% return the list of issues of the given project
list(Project_name) -> list(Project_name, []).

list(Project_name, Options) ->
    case sqlite3:open(db, [ {file, Project_name ++ "/db"}]) of
        {ok, Pid} -> ok;
        {error, {already_started, Pid}} -> ok
    end,
    sqlite3:sql_exec(db, "select rowid, * from issue;").



