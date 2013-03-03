%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------

%%
%% @doc Module for HTML rendering of TKE
%%
%% 
%%
%%

-module(tke_rendering_html).

-export([resource_not_found/0, list_issues/3]).
-export([show_issue/4]).
-export([login_page/0]).

% Return the ehtml structure for the table of issues
% The rowid must be present
% Issues = List of proplists
format_table_of_issues(Columns, Issues) ->
    %log:debug("format_table_of_issues: Columns=~p, Rows=~p", [Columns, Rows]),
    Head_ehtml = format_row(undefined, Columns, Columns, th),
    Rows_ehtml = format_table_rows(Columns, Issues), % /!\ extra rowid is in Rows
    %log:debug("Head_ehtml=~p", [Head_ehtml]),
    %log:debug("Rows_ehtml=~p", [Rows_ehtml]),
    {ehtml, { table, [{class, "list"}], [Head_ehtml, Rows_ehtml]}}.

date_to_string(undefined) -> "(undefined)";
date_to_string({{Y, Month, D},{H, Min, S}}) ->
    L = io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                              [Y, Month, D, H, Min, S]),
                              lists:flatten(L);
date_to_string(Timestamp) ->
    {{Y, Month, D},{H, Min, S}} = calendar:universal_time_to_local_time(Timestamp),
    date_to_string({{Y, Month, D},{H, Min, S}}).

to_string(X) when is_list(X) -> X;
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_integer(X) -> [R] = io_lib:format("~B", [X]), R;
to_string(X) when is_float(X) -> [R] = io_lib:format("~.2f", [X]), R;
to_string(null) -> "(null)";
to_string(undefined) -> "(undefined)";
to_string({{A, B, C}, {D, E, F}}) -> date_to_string({{A, B, C}, {D, E, F}});
to_string(X) -> io_lib:format("~s", [X]).

to_action_string([], Str) -> Str;
to_action_string(new, _Str) -> "create";
to_action_string([{Old, New} | Rest], Str) ->
    to_action_string(Rest, to_string(Str) ++ " " ++ to_string(Old)
        ++ " -> " ++ to_string(New) ++ "<br>\n").

format_cell(undefined, Column_name, _Column_value) -> atom_to_list(Column_name);
format_cell(Rowid, title, Column_value) ->
    {a, [{href, to_string(Rowid)}], to_string(Column_value)};
format_cell(_Rowid, mtime, Time = {{_Y, _Month, _D}, {_H, _Min, _Second}}) ->
    % compute duration since latest activity
    Now = calendar:universal_time(),
    Seconds_now = calendar:datetime_to_gregorian_seconds(Now),
    Seconds_event = calendar:datetime_to_gregorian_seconds(Time),
    % convert Column_value to gregorian seconds
    Dur = Seconds_now - Seconds_event,
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
format_row(Rowid, Column_names, Issue, Type_of_cell) ->
    C = format_row(Rowid, Column_names, Issue, [], Type_of_cell),
    C2 = lists:reverse(C),
    {tr, [], C2}.

% ther should be as many column names as columns
format_row(_Rowid, [], _Issue, Acc, _Type_of_cell) -> Acc;
format_row(Rowid, [N | Other_names], Issue, Acc, Type_of_cell) ->
    C = proplists:get_value(N, Issue),
    Text = format_cell(Rowid, N, C),
    This_cell = {Type_of_cell, [], Text},
    Acc2 = [This_cell | Acc],
    format_row(Rowid, Other_names, Issue, Acc2, Type_of_cell).
    

% Each row contains first the rowid, which must not be displayed
% Columns do not contain this first one.
% rowid may also be present in the other columns, if requested for display
format_table_rows(Columns, Issues) ->
    R = format_table_rows(Columns, Issues, []),
    lists:reverse(R). % put rows in correct order

format_table_rows(_Columns, [], Acc) -> Acc;
format_table_rows(Columns, [Issue | Others], Acc) ->
    Rowid = proplists:get_value(id, Issue),
    Acc2 = [format_row(Rowid, Columns, Issue, td) | Acc],
    format_table_rows(Columns, Others, Acc2).

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
            {th, [], "Date: " ++ date_to_string(Date)},
            {th, [], "Bouton DELETE"}
        ]},
    case Text of
        "" -> Contents = [];
        _Else ->
            Contents = {tr, [], {td, [{colspan, "4"}, {class, "content"}],
                                 {pre, [], to_string(Text)}}}
    end,
    messages(Messages, [Contents, Head | Acc]).

history([], Acc) ->
    First = {tr, [], {th, [{colspan, "4"}, {class, "header"}], "History"}},
    Second = {tr, [], [
                {th, [], "Date"},
                {th, [], "Author"},
                {th, [], "Action"}
            ]},
    {ehtml, {table, [{class, "history"}], [First, Second|lists:reverse(Acc)]}};
history([H | History], Acc) ->
    Author = proplists:get_value(author, H),
    Ctime = proplists:get_value(ctime, H),
    Action = proplists:get_value(action, H),
    Action_str = to_action_string(Action, ""),
    log:debug("Action_str=~p", [Action_str]),
    Contents = {tr, [], [
            {td, [], date_to_string(Ctime)},
            {td, [], to_string(Author)},
            {td, [], Action_str}
        ]},
    history(History, [Contents | Acc]).


%% Diff = [{Old, New}]
print_diff(undefined, []) -> [];
print_diff([], Acc) ->
    {tr, [], {td, [{colspan, "4"}, {class, "diff"}],
              string:join(Acc, ", ")}};
print_diff([{Old_value, New_value} | Rest_diff], Acc) ->
    Text = to_string(Old_value) ++ " -> " ++ to_string(New_value),
    print_diff(Rest_diff, [Text | Acc]).




%% HTML for edition of field (<input>)
%% Name = atom() : specify the field
%% Value = term() : current value
%% Properties = tuple() : select, select_multiple, textarea, none
edition_field(_P, Name, Value, {}) ->
    % TODO get list of values for lists, get size, etc.
    { input, [{name, atom_to_list(Name)},
            {type, "text"}, {value, to_string(Value)}], ""};

edition_field(Project, Name, Value, {textarea}) ->
    edition_textarea(Project, Name, Value);

edition_field(Project, Name, Value, {select, List}) ->
    edition_select(Project, Name, Value, List, [], multiple_no);

edition_field(Project, Name, Value, {select_multiple, List}) ->
    edition_select(Project, Name, Value, List, [], multiple_yes).

%% <select>
edition_select(_P, Name, _Value, [], Acc, multiple_no) ->
    {select, [{name, Name}], lists:reverse(Acc)};

edition_select(_P, Name, _Value, [], Acc, multiple_yes) ->
    {select, [{name, Name}, {multiple, "multiple"}], lists:reverse(Acc)};


%% field for selecting among list of users
edition_select(Project, Name, Value, user, Acc, Multiple) ->
    edition_select(Project, Name, Value,
                   tke_user:get_list_of_users(Project), Acc, Multiple);

edition_select(Project, Name, Value, [Option | Rest], Acc, multiple_yes) ->
    % if Option is in the list given by Value, then select this option
    % (or if Option == Value as well)
    log:debug("Value=~p", [Value]),
    if 
        is_list(Value) and (length(Value) > 0) ->
            [A | _B] = Value,
            if is_list(A) -> % case where Value is list(list(char))
                case lists:member(Option, Value) of
                    true -> Attr = [{selected, "selected"}];
                    _Else2 -> Attr = []
                end;

            Option == Value -> % case where Value is list(char)
                Attr = [{selected, "selected"}];

            true -> Attr = []
            end;

        true -> % may be 'undefined' for creation of a new issue
            Attr = []
    end,
    Html_opt = {option, [{value, Option}|Attr], Option},
    edition_select(Project, Name, Value, Rest, [Html_opt | Acc], multiple_yes);

edition_select(Project, Name, Value, [Option | Rest], Acc, multiple_no) ->
    case Option of
        Value -> % pre-select this one
            Attr = [{selected, "selected"}];
        _Else -> Attr = []
    end,
    Html_opt = {option, [{value, Option}|Attr], Option},
    edition_select(Project, Name, Value, Rest, [Html_opt | Acc], multiple_no).

%% <textarea>
edition_textarea(_P, Name, Value) ->
    {textarea, [{name, Name}], to_string(Value)}.

% return EHTML for displaying a list of issues
list_issues(Project, Columns, Issues) ->
    [   header(Project),
        format_table_of_issues(Columns, Issues),
        footer(Project)
    ].

% return EHTML for diaplying issue
show_issue(Project, Issue, Messages, History) ->
    log:debug("show_issue..."),
    [   header(Project),
        title_issue(Issue),
        edition_form(Project, Issue),
        created_by(Issue),
        messages(Messages, []),
        history(History, []),
        footer(Project)
    ].

title_issue(Issue) ->
    case proplists:get_value(id, Issue) of
        undefined -> % case of new issue
            Title = "Create new issue";
        Id0 -> 
            Id = Id0,
            Title = "Issue " ++ Id
    end,
    {ehtml, {h1, [], Title}}.

created_by(Issue) ->
    Ctime = proplists:get_value(ctime, Issue),
    Author = proplists:get_value(author, Issue),
    Ctime_str = to_string(Ctime),
    Author_str = to_string(Author),
    if Ctime == undefined ->
            "";
        true ->
            {ehtml, {p, [], "Created on <b>" ++ Ctime_str ++ "</b> by <b>"
            ++ Author_str ++ "</b>"}}
    end.

edition_form(Project, Issue) ->
    {ehtml, {form, [
                {method, "post"},
                {action, ""},
                {enctype, "multipart/form-data"}
            ],

                {table, [{class, "form"}], [
                    details(Project, Issue, []),
                    edition_message(),
                    submit()
                ]}  % end of table
        }}.

% fields of an issue
details(_Project, [], Html_rows_acc) -> lists:reverse(Html_rows_acc);
% id is not editable
details(Project, [{Name, Value} | Others], Acc) ->
    case lists:member(Name, tke_db:get_columns_automatic()) of
        true -> % automatic fields are not editable
            details(Project, Others, Acc);
        _Else ->
            P = tke_db:get_column_properties(Project, Name),
            Ehtml = {tr, [], [{th, [], atom_to_list(Name)},
                              {td, [], edition_field(Project, Name, Value, P)}]},
            details(Project, Others, [Ehtml | Acc])
    end.

edition_message() ->
    {tr, [], [{th, [], "Description: "},
              {td, [],
                  {textarea, [
                          {name, message},
                          {wrap, "hard"}, {rows, "10"}, {cols, "80"}
                      ], ""}
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

login_page() ->
    {ok, Html} = file:read_file("tke_users/login.html"),
    {html, Html}.
