%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Format HTML pages
%%%

-module(tke_html).

-compile(export_all).

format_table_of_issues([ {columns, Columns}, {rows, Rows} ]) ->
    Head_ehtml = format_table_header(Columns),
    Rows_ehtml = format_table_rows(Rows),
    {ehtml, { table, [{class, "t_issues"}], [Head_ehtml, Rows_ehtml]}}.

format_table_header(Columns) ->
    C = format_table_header(Columns, []),
    C2 = lists:reverse(C),
    {tr, [{class, "t_tr_head"}], C2}.

format_table_header([], Acc) -> Acc;
format_table_header([C | Other_columns], Acc) ->
    Acc2 = [{ td, [{class, "t_th_head"}], C} | Acc ],
    format_table_header(Other_columns, Acc2).
    

format_table_rows(Rows) ->
    R = format_table_rows(Rows, []),
    lists:reverse(R).

format_table_rows([], Acc) -> Acc;
format_table_rows([Current_row | Other], Acc) ->
    Cols = tuple_to_list(Current_row),
    Acc2 = [format_table_header(Cols) | Acc],
    format_table_rows(Other, Acc2).
