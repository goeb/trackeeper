%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Format HTML pages
%%%

-module(tke_html).

-compile(export_all).

format_table_of_issues([ {columns, Columns}, {rows, Rows} ]) ->
    Head_ehtml = format_table_row(Columns, Columns, th),
    Rows_ehtml = format_table_rows(Columns, Rows),
    {ehtml, { table, [{class, "list"}], [Head_ehtml, Rows_ehtml]}}.

% Type_of_cell = td | th
format_table_row(Column_names, Columns, Type_of_cell) ->
    C = format_table_row(Column_names, Columns, [], Type_of_cell),
    C2 = lists:reverse(C),
    {tr, [{class, "t_tr_head"}], C2}.

to_string(X) when is_list(X) -> X;
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_integer(X) -> io_lib:format("~B", [X]);
to_string(null) -> "(null)".

% ther should be as many column names as columns
format_table_row([], [], Acc, _Type_of_cell) -> Acc;
format_table_row([N | Other_names], [C | Other_columns], Acc, Type_of_cell) ->
    case N of 
        "title" -> Text = {a, [{href, "xxx"}], to_string(C)};
        _else -> Text = to_string(C)
    end,
    This_cell = { Type_of_cell, [{class, "t_td"}], Text},
    Acc2 = [This_cell | Acc],
    format_table_row(Other_names, Other_columns, Acc2, Type_of_cell).
    

format_table_rows(Columns, Rows) ->
    R = format_table_rows(Columns, Rows, []),
    lists:reverse(R). % put rows in correct order

format_table_rows(Columns, [], Acc) -> Acc;
format_table_rows(Column_names, [Current_row | Other], Acc) ->
    Cols = tuple_to_list(Current_row),
    Acc2 = [format_table_row(Column_names, Cols, td) | Acc],
    format_table_rows(Column_names, Other, Acc2).
