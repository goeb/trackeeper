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
to_string(X) when is_integer(X) -> [R] = io_lib:format("~B", [X]), R;
to_string(X) when is_float(X) -> [R] = io_lib:format("~.2f", [X]), R;
to_string(null) -> "(null)";
to_string(undefined) -> "(undefined)".

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

messages([], Acc) ->
    First = {tr, [], {th, [{colspan, "4"}, {class, "header"}], "Messages"}},
    {ehtml, {table, [{class, "messages"}], [First|lists:reverse(Acc)]}};
messages([M | Messages], Acc) ->
    Author = proplists:get_value(author, M),
    Date = proplists:get_value(ctime, M),
    Id = proplists:get_value(id, M),
    Text = proplists:get_value(text, M, "no text"),
    Head = {tr, [], [
            {th, [], "msg." ++ to_string(Id)},
            {th, [], "Author: " ++ to_string(Author)},
            {th, [], "Date: " ++ to_string(Date)},
            {th, [], "Bouton DELETE"}
        ]},
    Contents = {tr, [], {td, [{colspan, "4"}, {class, "content"}],
            {pre, [], Text}}},
    messages(Messages, [Contents, Head | Acc]).


%% HTML for edition of field (<input>)
%% Name = atom() : specify the field
%% Value = term() : current value
edition_field(_Name, Value) ->
    % TODO get list of values for lists, get size, etc.
    { input, [{type, "text"}, {value, to_string(Value)}], ""}.

% return EHTML for diaplying issue
show_issue(Project, Issue, Messages) ->
    log:debug("show_issue..."),
    [   header(Project),
        edition_form(Issue),
        messages(Messages, []),
        footer(Project)
    ].

edition_form(Issue) ->
    {ehtml, {form, [{method, "post"}, {action, ""}, {enctype, "multipart/form-data"}],
                {table, [{class, "form"}], [
                    details(Issue, []),
                    edition_message(),
                    submit()
                ]}  % end of table
        }}.

details([], Html_rows_acc) -> lists:reverse(Html_rows_acc);
details([{Name, Value} | Others], Acc) ->
    Ehtml = {tr, [], [{th, [], atom_to_list(Name)},
                      {td, [], edition_field(Name, Value)}]},
    details(Others, [Ehtml | Acc]).

edition_message() ->
    {tr, [], [{th, [], "Description: "},
              {td, [],
                  {textarea, [{wrap, "hard"}, {rows, "10"}, {cols, "80"}], "Enter your message"}
              }]
      }.

submit() ->
    {tr, [], [{th, [], ""},
              {td, [], {input, [{type, "submit"}, {value, "Submit"}], ""}}
        ]}.

header(Project) ->
    case file:read_file(Project ++ "/header.html") of
        {ok, Header} -> ok;
        {error, Reason} -> Header = "no header: " ++ atom_to_list(Reason)
    end,
    {html, Header}.

footer(Project) ->
    {ok, Footer} = file:read_file(Project ++ "/footer.html"),
    {html, Footer}.
