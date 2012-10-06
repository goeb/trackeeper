%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Handle interactions with the database
%%%

%%% The table 'link' plays the role of a foreign keys table.
%%% No real foreign keys are used, because Sqlite3 does
%%% not support adding a column with foreign key to an existing table.
%%%
%%%


-module(tke_base).

-compile(export_all).

% return the list of issues of the given project
list(Project_name) -> list(Project_name, []).

% Return the list of issues
% Project_name: list(char)
%               directory of the project
% Options: [ {columns, [list(char), ...] }, ... ]
list(Project_name, Options) ->
    Cols = proplists:get_value(columns, Options),
    log:debug("list(~p, ~p)", [Project_name, Options]),
    Fd = sql_open(db, Project_name),
    case Cols of
        all -> Cols2 = get_column_names(db, issue);
        _else -> Cols2 = Cols
    end,

    Cols3 = ["rowid" | Cols2], % add always rowid first (even if already present)
    Sql_req = build_sql_list_request(db, Cols3),

    log:debug("sql_open Fd=~p", [Fd]),
    case Fd of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    [{columns, _C}, {rows, Rows}] = sql_exec(db, Sql_req),
    [{columns, Cols3}, {rows, Rows}]. % replace column names

% handle 'all' columns
get_column_names(Db, issue) ->
    Cols = sqlite3:table_info(Db, issue),
    Column_names = [ atom_to_list(Col) || {Col, Type} <- Cols],
    ["rowid" | Column_names].

% Build SQL request for the list of issues
% Db must be an opened Sqlite3 database handler
build_sql_list_request(Db, Columns) ->
    log:debug("build_sql_list_request/2(~p)", [[Db, Columns]]),
    % reverse columns as they will be reversed again soon
    build_sql_list_request(Db, lists:reverse(Columns), [], ["issue"], []).

% Build SQL request for the list of issues (with accumulator)
% Db must be an opened Sqlite3 database handler
build_sql_list_request(_Db, [], Sql_select, Sql_from, Sql_where) ->
    Sql = "SELECT " ++ string:join(Sql_select, ",") ++
        " FROM " ++ string:join(Sql_from, ","),
    case Sql_where of
        [] -> Sql ++ ";";
        Sql_where when is_list(Sql_where) ->
            Sql ++ " WHERE " ++ string:join(Sql_where, " AND ") ++ ";"
    end;
    
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
