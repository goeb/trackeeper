%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Format HTML pages
%%%

-module(tke_html).

-compile(export_all).

format_table_of_issues([ {columns, Columns}, {rows, Rows} ]) ->
    Head_ehtml = format_table_row(Columns, th),
    Rows_ehtml = format_table_rows(Rows),
    {ehtml, { table, [{class, "list"}], [Head_ehtml, Rows_ehtml]}}.

% Type_of_cell = td | th
format_table_row(Columns, Type_of_cell) ->
    C = format_table_row(Columns, [], Type_of_cell),
    C2 = lists:reverse(C),
    {tr, [{class, "t_tr_head"}], C2}.

to_string(X) when is_list(X) -> X;
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_integer(X) -> io_lib:format("~B", [X]);
to_string(null) -> "(null)".

format_table_row([], Acc, _Type_of_cell) -> Acc;
format_table_row([C | Other_columns], Acc, Type_of_cell) ->
    Acc2 = [{ Type_of_cell, [{class, "t_td"}], to_string(C)} | Acc ],
    format_table_row(Other_columns, Acc2, Type_of_cell).
    

format_table_rows(Rows) ->
    R = format_table_rows(Rows, []),
    lists:reverse(R). % put rows in correct order

format_table_rows([], Acc) -> Acc;
format_table_rows([Current_row | Other], Acc) ->
    Cols = tuple_to_list(Current_row),
    Acc2 = [format_table_row(Cols, td) | Acc],
    format_table_rows(Other, Acc2).
