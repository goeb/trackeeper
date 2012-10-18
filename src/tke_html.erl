%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Format HTML pages
%%%

-module(tke_html).

-compile(export_all).

% Return the ehtml structure for the table of issues
% The rowid is always the first item of rows, as we need it for hyper-links
format_table_of_issues([ {columns, ["rowid" | Columns]}, {rows, Rows} ]) ->
    %log:debug("format_table_of_issues: Columns=~p, Rows=~p", [Columns, Rows]),
    Head_ehtml = format_row(undefined, Columns, Columns, th),
    Rows_ehtml = format_table_rows(Columns, Rows), % /!\ extra rowid is in Rows
    {ehtml, { table, [{class, "list"}], [Head_ehtml, Rows_ehtml]}}.

to_string(X) when is_list(X) -> X;
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_integer(X) -> io_lib:format("~B", [X]);
to_string(X) when is_float(X) -> io_lib:format("~.2f", [X]);
to_string(null) -> "(null)".

format_cell(undefined, _Column_name, Column_value) -> Column_value;
format_cell(Rowid, "title", Column_value) ->
    {a, [{href, to_string(Rowid)}], to_string(Column_value)};
format_cell(_Rowid, "date", null) -> to_string(null);
format_cell(_Rowid, "date", Column_value) ->
    % compute duration since latest activity
    Now = calendar:universal_time(),
    Seconds = calendar:datetime_to_gregorian_seconds(Now),
    % convert Column_value to gregorian seconds
    Val = Column_value + 719528*86400,
    Dur = Seconds - Val,
    case Dur of 
        Dur when Dur < 60 -> T = to_string(Dur) ++ " s";
        Dur when Dur < 3600 -> T = to_string(Dur/60) ++ " min";
        Dur when Dur < 86400 -> T = to_string(Dur/3600) ++ " h";
        Dur when Dur < 2592000 -> T = to_string(Dur/86400) ++ " days";
        Dur when Dur < 31536000 -> T = to_string(Dur/2592000) ++ " months";
        Dur -> T = to_string(Dur/31536000) ++ " years"
    end,
    T ++ " ago";
format_cell(_Rowid, _Column_name, Column_value) -> to_string(Column_value).


% Type_of_cell = td | th
format_row(Rowid, Column_names, Columns, Type_of_cell) ->
    C = format_row(Rowid, Column_names, Columns, [], Type_of_cell),
    C2 = lists:reverse(C),
    {tr, [{class, "t_tr_head"}], C2}.

% ther should be as many column names as columns
format_row(_Rowid, [], [], Acc, _Type_of_cell) -> Acc;
format_row(Rowid, [N | Other_names], [C | Other_columns], Acc, Type_of_cell) ->
    Text = format_cell(Rowid, N, C),
    This_cell = { Type_of_cell, [{class, "t_td"}], Text},
    Acc2 = [This_cell | Acc],
    format_row(Rowid, Other_names, Other_columns, Acc2, Type_of_cell).
    

% Each row contains first the rowid, which must not be displayed
% Columns do not contain this first one.
% rowid may also be present in the other columns, if requested for display
format_table_rows(Columns, Rows) ->
    R = format_table_rows(Columns, Rows, []),
    lists:reverse(R). % put rows in correct order

format_table_rows(_Columns, [], Acc) -> Acc;
format_table_rows(Column_names, [Current_row | Other], Acc) ->
    [Rowid | Cols] = tuple_to_list(Current_row),
    Acc2 = [format_row(Rowid, Column_names, Cols, td) | Acc],
    format_table_rows(Column_names, Other, Acc2).

resource_not_found() ->
    [{html, "404 - Resource not found"}, {status, 404}].

messages(_M) -> {html, "messages xxxxxxx"}.
details(Issue) -> 
    Field_names = proplists:get_value(columns, Issue),
    [Field_values] = proplists:get_value(rows, Issue),
    % field names is a list whereas field values is a tuple
    Field_values2 = tuple_to_list(Field_values),
    Ehtml = format_details(Field_names, Field_values2, []),
    Ehtml.
    %{html, io_lib:format("Issue_data=~p", [Issue])}.

format_details([], [], Html_rows_acc) ->
    {ehtml, {table, [{class, "form"}], Html_rows_acc}};
format_details([Name | Other_fields], [Value | Other_values], Acc) ->
    Ehtml = { tr, [], [{th, [], Name}, {td, [], to_string(Value)}]},
    format_details(Other_fields, Other_values, [Ehtml | Acc]).


% return EHTML for diaplying issue
show_issue(Project, [Issue, Messages]) ->
    [header(Project),
     details(Issue),
     messages(Messages),
     footer(Project)
    ].


header(Project) ->
    {ok, Header} = file:read_file(Project ++ "/header.html"),
    {html, Header}.

footer(Project) ->
    {ok, Footer} = file:read_file(Project ++ "/footer.html"),
    {html, Footer}.
