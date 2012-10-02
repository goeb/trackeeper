%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Handle interactions with the database
%%%

-module(tke_base).

-compile(export_all).

% return the list of issues of the given project
list(Project_name) -> list(Project_name, []).

% Project_name: list(char)
%               directory of the project
% Options: [ {columns, [list(char), ...] }, ... ]
list(Project_name, Options) ->
    Cols = proplists:get_value(columns, Options),
    log:debug("list(~p, ~p)", [Project_name, Options]),
    Fd = sql_open(db, Project_name),
    Sql_req = build_sql_list_request(db, Cols),
    log:debug("sql_open Fd=~p", [Fd]),
    case Fd of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    %sqlite3:sql_exec(db, "select rowid, * from issue;").
    sql_exec(db, Sql_req).

% Db must be an opened Sqlite3 database handler
build_sql_list_request(Db, Columns) ->
    log:debug("build_sql_list_request/2(~p)", [[Db, Columns]]),
    build_sql_list_request(Db, Columns, [], ["issue"], []).

% Db must be an opened Sqlite3 database handler
build_sql_list_request(_Db, [], Sql_select, Sql_from, Sql_where) ->
    Sql = "SELECT " ++ string:join(Sql_select, ",") ++
        " FROM " ++ string:join(Sql_from, ",") ++
        " WHERE " ++ string:join(Sql_where, " AND ") ++ ";";
    
build_sql_list_request(Db, [Col | Others], Sql_select, Sql_from, Sql_where) ->
    log:debug("build_sql_list_request(~p)", [[Db, [Col | Others], Sql_select, Sql_where]]),
    % is Col a link to another table ?
    Sql = "select * from link where column='" ++ Col ++ "';",
    Sql_result = sql_exec(Db, Sql),
    [{columns, Cols}, {rows, Rows}] = Sql_result,
    case Rows of
        [] -> % no link. use raw column of table 'issue'
            Sql_select2 = ["issue." ++ Col | Sql_select],
            Sql_where2 = Sql_where, % no change
            Sql_from2 = Sql_from;
        [Row] -> % one single row
            Row_txt = [ binary_to_list(X) || X <- tuple_to_list(Row) ],
            [Table_name, Col, Table_ref, Col_ref] = Row_txt,
            % add a where clause and a joint
            Sql_select2 = [Table_ref ++ "." ++ Col_ref | Sql_select],
            Sql_where2 = ["issue." ++ Col ++ "=" ++ Table_ref ++ ".rowid" | Sql_where],
            Sql_from2 = [Table_ref | Sql_from]
    end,
    build_sql_list_request(Db, Others, Sql_select2, Sql_from2, Sql_where2).
    

sql_exec(Db, Sql_req) ->
    log:debug("SQL: " ++ Sql_req),
    R = sqlite3:sql_exec(Db, Sql_req),
    log:debug("SQL-result: ~p", [R]),
    R.

sql_open(Db, Project_name) ->
    Path = Project_name ++ "/db",
    log:debug("SQL-open: " ++ Path),
    sqlite3:open(Db, [{file, Path}]).
