%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Manage TKE database.
%%%
%%%
%%%
%%% Example:
%%% tke_db:get(tke, message, 4).
%%%     [{id, 4}, {issue, 1}, {author, undefined},
%%%              {ctime, undefined}, {contents, undefined}]
%%% tke_db:get(tke, issue, 2).
%%%     ...
%%% tke_db:update(tke, issue, [{id, undefined}, ...]).
%%%     {ok,#message{id = 4,issue = 1,author = undefined,
%%%          ctime = undefined,contents = undefined}}



-module(tke_db).
-behaviour(gen_server).

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/3, search/3]).
-export([get_columns_automatic/0, get_column_properties/2]).

-include("tke_db.hrl").
-record(project, {name, issues, messages, history, structure}).
-record(history, {id, issue, author, ctime, action}).

% action = [new_issue,
%           {add_message, 34},
%           {delete_message, 434}, 
%           {add_file, 2},
%           {delete_file, 2},
%           {change_field, {Old, New}},

%% API -------------------

% start as many processes as there are projects in the directory
start(Project) ->
    gen_server:start_link({local, registered_name(Project)},
                          tke_db, Project, []).

stop(Project) -> gen_server:cast(registered_name(Project), stop).

%% Project = atom() | list()
%% Table = issue | message
get(Project, Table, N) when is_list(Project) ->
    gen_server:call(registered_name(Project), {get, Table, N}).

%% if new issue, then create and return id.
%% if existing issue, then update
%% Issue contains the message
update(Project, issue, Issue) ->
    log:debug("update0: "),
    gen_server:call(registered_name(Project), {update, Issue}).

search(Project, Table, Search) ->
    gen_server:call(registered_name(Project), {search, Table, Search}).

%% Get properties of a column
%% (such as multi-select, etc.)
get_column_properties(Project, Column) ->
    gen_server:call(registered_name(Project), {get_column_properties, Column}).

%% Internals -------------------


registered_name(Project) ->
    list_to_atom("tke_" ++ Project).
    

%% Project = list(char)
init(Project) ->
    log:debug("Loading project ~p", [Project]),
    % TODO load from disk and populate an ets table
    Ctx = load(Project),
    log:debug("Loading project ~p completed", [Project]),
    {ok, Ctx}.

handle_call({get, issue, empty}, _From, Ctx) ->
    E = get_empty_issue(Ctx),
    log:debug("get_empty_issue: returned ~p", [E]),
    {reply, E, Ctx};
handle_call({get, issue, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.issues, N) of
        [I0] -> I = convert_to_proplist(Ctx, I0);
        [] -> I = undefined
    end,
    {reply, I, Ctx};
handle_call({get, message, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.messages, N) of
        [M0] -> %log:debug("found message ~p", [M0]),
            M = convert_to_proplist(Ctx, M0);
        [] -> M = undefined
    end,
    {reply, M, Ctx};
%% create new issue
%% Issue contains the message
handle_call({update, Issue}, _From, Ctx) ->
    log:debug("update: Issue=~p", [Issue]),
    Id0 = proplists:get_value(id, Issue),
    % TODO the diff of the issue wrt. to previous value
    Timestamp = get_timestamp(),
    case Id0 of 
        undefined ->
            Id = get_new_id(Ctx#project.issues),
            % ctime
            Ctime = Timestamp,
            % author
            Author = tke_user:get_author(),
            Issue2 = proplists:delete(id, Issue), % replace id
            Issue21 = [{id, Id}, {ctime, Ctime}, {author, Author},
                      {mtime, Timestamp} | Issue2];
        Id0 ->
            Id = Id0,
            [Old_issue_0] = ets:lookup(Ctx#project.issues, Id0),
            Oi = convert_to_proplist(Ctx, Old_issue_0),
            % report automatic fields of existing issue
            Ctime = proplists:get_value(ctime, Oi),
            Mtime = proplists:get_value(mtime, Oi),
            Author = proplists:get_value(author, Oi),
            Issue21 = [{ctime, Ctime}, {author, Author}, {mtime, Mtime}
                | Issue]
    end,
    Issue3 = [{mtime, Timestamp} | Issue21],
    % convert proplist to entry (~ record)
    I = convert_to_entry(Ctx, issue, Issue3),

    % make the diff (the summary of changes)
    case Id0 of
        undefined -> Diff = new;
        _Else ->
            [Old_issue] = ets:lookup(Ctx#project.issues, Id0),
            Diff = make_diff(tuple_to_list(Old_issue), tuple_to_list(I), []),
            log:debug("Diff: ~p", [Diff])
    end,

    case Diff of 
        [] -> % nothing new. do not update anything
            log:debug("nothing updated");
        Diff -> 
            ets:insert(Ctx#project.issues, I),
            log:debug("going to sync..."),
            sync(Ctx, I),
            % add history log
            add_history(Id, Diff, Timestamp, Ctx)
    end,
    % now add the message
    add_message(Id, Issue, Timestamp, Ctx),

    {reply, {ok, Id}, Ctx};

% Search : proplist
%   key 'columns' : list of columns needed in the return value
%   key 'pattern' : pattern for selective search TODO
handle_call({search, issue, Search}, _From, Ctx) ->
    log:debug("search issue: Search=~p", [Search]),
    % for now, return the list of all issues
    % TODO filter
    Pattern_l = lists:duplicate(length(get_columns(Ctx, issue)), '_'),
    Pattern = list_to_tuple([issue | Pattern_l]),
    Issues = ets:match_object(Ctx#project.issues, Pattern),
    I_list = [convert_to_proplist(Ctx, I) || I <- Issues],
    % now keep only the needed columns
    Columns = proplists:get_value(columns, Search),
    case Columns of
        all -> Needed_columns = get_columns(Ctx, issue);
        Needed_columns -> ok
    end,

    % do the sorting
    Sort = proplists:get_value(sort, Search),
    log:debug("search: Sort=~p", [Sort]),
    I_list2 = sort(I_list, Sort),
    {reply, {Needed_columns, I_list2}, Ctx};

%% Return messages that belong to the given issue id
handle_call({search, message, Issue_id}, _From, Ctx) ->
    Pattern = #message{issue=Issue_id, _ = '_'},
    log:debug("search ~p", [Pattern]),
    Messages = ets:match_object(Ctx#project.messages, Pattern),
    Mlist = [convert_to_proplist(Ctx, M) || M <- Messages],
    M_list2 = sort(Mlist, [id]),
    {reply, M_list2, Ctx};

%% Return history of a given issue
handle_call({search, history, Issue_id}, _From, Ctx) ->
    Pattern = #history{issue=Issue_id, _ = '_'},
    log:debug("search ~p", [Pattern]),
    History = ets:match_object(Ctx#project.history, Pattern),
    H_list = [convert_to_proplist(Ctx, H) || H <- History],
    H_list2 = sort(H_list, [id]),
    {reply, H_list2, Ctx};

handle_call({get_column_properties, Column}, _From, Ctx) ->
    Columns = proplists:get_value(issue_columns, Ctx#project.structure),
    Properties = proplists:get_value(Column, Columns),
    {reply, Properties, Ctx}.


%% Return proplists of issue, with all fields undefined
get_empty_issue(Ctx) ->
    Cols = get_columns(Ctx, issue),
    Values = lists:duplicate(length(Cols), undefined),
    I = list_to_tuple([issue | Values]), 
    convert_to_proplist(Ctx, I).

%% List : list of proplists
%% For each proplist, delete columns that are not mentioned in Columns
keep_columns([], _Columns, Acc) -> Acc;
keep_columns([Plist | Others], Columns, Acc) ->
    log:debug("keep_columns: Plist=~p, Columns=~p", [Plist, Columns]),
    {Lists, _Rest} = proplists:split(Plist, Columns),
    keep_columns(Others, Columns, [Lists | Acc]).


handle_cast(stop, Ctx) -> {stop, normal, Ctx};
handle_cast(_X, Y) -> {noreply, Y}.

%% File access functions
load(Project) ->
    Structure = load_project_file(Project),
    Issue_table = ets:new(issue,[private, {keypos, 2}]),
    Message_table = ets:new(message,[private, {keypos, 2}]),
    History_table = ets:new(history,[private, {keypos, 2}]),
    Ctx = #project{name=Project,
                   issues=Issue_table,
                   messages=Message_table,
                   history=History_table,
                   structure=Structure},
    load_issues(Ctx),
    Ctx.

load_project_file(Project) ->
    File = Project ++ "/project",
    {ok, Binary} = file:read_file(File),
    Term = decode_contents(Binary),
    Term.

load_issues(Ctx) ->
    {ok, Files} = file:list_dir(Ctx#project.name),
    Dirs = [Ctx#project.name ++ "/" ++ File || File <- Files],
    load_issues_from_dirs(Dirs, Ctx).

load_issues_from_dirs([], _Ctx) -> ok;
load_issues_from_dirs([Dir | Others], Ctx) ->
    File = Dir ++ "/issue",
    case file:read_file(File) of
        {error, _Reason} -> ok;
        {ok, Binary} ->
            Term = decode_contents(Binary),
            % convert proplist to record-like tuple
            I = convert_to_entry(Ctx, issue, Term),
            ets:insert(Ctx#project.issues, I)
            %log:debug("Issue ~p loaded.", [File])
    end,
    load_messages_and_history(Dir, Ctx#project.messages, Ctx#project.history),
    load_issues_from_dirs(Others, Ctx).

decode_contents(Binary) ->
    S = binary_to_list(Binary),
    {ok, Tokens, _EndLocation} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

load_messages_and_history(Dir, Messages, History) -> 
    case file:list_dir(Dir) of
        {ok, Files} -> load_from_files(Dir, Files, Messages, History);
        {error, _Reason} -> ok
    end.

%% Messages = ETS table containing messages
%% History = ETS table containing history events
load_from_files(_Dir, [], _Messages, _History) -> ok;
% Consider files starting with "msg."
load_from_files(Dir, [[$m, $s, $g, $. | Id] | Others], Messages, His) ->
    File = Dir ++ "/msg." ++ Id,
    %log:debug("Message ~p", [File]),
    case file:read_file(File) of
        {error, _Reason} -> log:debug("Message ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            case merge_to_record(message, Term) of % TODO is this really useful?
                {error, Reason} -> log:error("Cannot load message ~p: ~p",
                                             [Id, Reason]);
                {ok, T} -> ets:insert(Messages, T)
            end
    end,
    load_from_files(Dir, Others, Messages, His);

%% Load History
load_from_files(Dir, [[$h, $i, $s, $. | Id] | Others], Messages, History) ->
    File = Dir ++ "/his." ++ Id,
    case file:read_file(File) of
        {error, _Reason} -> log:debug("History ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            ets:insert(History, Term)
    end,
    load_from_files(Dir, Others, Messages, History);

% other files, not starting with "msg."
load_from_files(Dir, [_F | Others], Messages, History) ->
    load_from_files(Dir, Others, Messages, History).



%% write to disk what has been modified
sync(Ctx, I) when element(1, I) == issue -> 
    log:debug("syncing..."),
    Id = integer_to_list(get_value(Ctx, issue, id, I)),
    Dirname = Ctx#project.name ++ "/" ++ Id,
    file:make_dir(Dirname),
    Filename = Dirname ++ "/issue",
    % convert record-like tuple to proplist
    Prop_i = convert_to_proplist(Ctx, I),
    sync_file(Filename, Prop_i);

sync(Ctx, M = #message{}) ->
    log:debug("syncing message: ~p", [M]),
    Issue = integer_to_list(M#message.issue),
    Id = integer_to_list(M#message.id),
    Dirname = Ctx#project.name ++ "/" ++ Issue,
    Filename = Dirname ++ "/msg." ++ Id,
    sync_file(Filename, M);

sync(Ctx, H = #history{}) ->
    log:debug("syncing history: ~p", [H]),
    Issue = integer_to_list(H#history.issue),
    Id = integer_to_list(H#history.id),
    Dirname = Ctx#project.name ++ "/" ++ Issue,
    Filename = Dirname ++ "/his." ++ Id,
    sync_file(Filename, H).


sync_file(Filename, Data) ->
    Str = io_lib:format("~p.", [Data]),
    Bytes = list_to_binary(Str),
    X=file:write_file(Filename, Bytes),
    log:debug("syncing...~p, Filename=~p", [X, Filename]),
    X.



code_change(_, _, _) -> ok.
handle_info(_, _) -> ok.
terminate(shutdown, _State) -> ok.

%% Get new id functions for issue or message
get_new_id(Table) -> get_max_id(Table, ets:first(Table), 0) + 1.

get_max_id(_Table, '$end_of_table', N) -> N;
get_max_id(Table, Key, N) ->
    case Key > N of
        true -> Max = Key;
        _Else -> Max = N
    end,
    get_max_id(Table, ets:next(Table, Key), Max).

convert_to_proplist(Ctx, I) when element(1, I) == issue ->
    lists:zip(get_columns(Ctx, issue), tl(tuple_to_list(I)));
convert_to_proplist(_Ctx, M = #message{}) ->
    lists:zip(record_info(fields, message), tl(tuple_to_list(M)));
convert_to_proplist(_Ctx, H = #history{}) ->
    lists:zip(record_info(fields, history), tl(tuple_to_list(H))).

convert_to_record([], _Proplist, Record) ->
    list_to_tuple(lists:reverse(Record));
convert_to_record([Key | Others], Proplist, Record) ->
    Value = proplists:get_value(Key, Proplist),
    convert_to_record(Others, Proplist, [Value|Record]).

%% Project = list(char)  = Name of project
%% I       = proplists() = Issue to be converted
convert_to_entry(Ctx, issue, I) ->
    Keys = get_columns(Ctx, issue),
    convert_to_record(Keys, I, [issue]).

%% Sort a proplist according to a key
sort(I_list, undefined) -> I_list;
%% Col = atom() = Key used for the sorting of the proplist
sort(I_list, [Col]) ->
    % less than or equal function
    Lte = fun(A, B) ->
            proplists:get_value(Col, A) < proplists:get_value(Col, B) end,
    lists:sort(Lte, I_list);
   
%% TODO multi-column  sorting
sort(_I_list, _Sort) -> todo.

get_timestamp() ->
    TS = os:timestamp(),
    calendar:now_to_universal_time(TS).

%% Issue_id : id of related issue
%% Message = proplists() (contains also Issue info, but not needed here)
%% Ctx : context of the server    
add_message(Issue_id, Message, Timestamp, Ctx) ->
    Text = proplists:get_value(message, Message),
    Text_stripped = string:strip(Text),
    Id = get_new_id(Ctx#project.messages),
    M = #message{id=Id,
                 issue=Issue_id,
                 author="John Doe",
                 ctime=Timestamp,
                 text=Text_stripped},

    case Text_stripped of 
        "" -> % do not insert message in base (nothing has changed)
            log:debug("message not added");
        Text_stripped ->
            ets:insert(Ctx#project.messages, M),
            sync(Ctx, M)
    end,
    ok.

add_history(_Issue_id, [], _Timestamp, _Ctx) -> ok; % no diff, then do nothing
add_history(Issue_id, Diff, Timestamp, Ctx) ->
    Id = get_new_id(Ctx#project.history),
    H = #history{id=Id,
                 issue=Issue_id,
                 author="John Doe", % TODO
                 ctime=Timestamp,
                 action=Diff},
    ets:insert(Ctx#project.history, H),
    sync(Ctx, H).


%% Term = tuple()
merge_to_record(message, Term) ->
    case element(1, Term) of
        message -> 
            N = size(Term) - size(#message{}),
            case N of
                N when N > 0 -> T = delete_elements(Term, N);
                N when N == 0 -> T = Term;
                N when N < 0 -> T = add_elements(Term, -N)
            end,
            {ok, T};
        _Other -> {error, "Not a message structure"}
    end.

delete_elements(Tuple, 0) when is_tuple(Tuple) -> Tuple;
delete_elements(Tuple, N) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Size = length(List),
    L2 = lists:sublist(List, 1, Size - N),
    list_to_tuple(L2).

add_elements(Tuple, 0) -> Tuple;
add_elements(Tuple, N) ->
    add_elements(erlang:append_element(Tuple, undefined), N-1).

%% Stucture of table issue
%% Columns are read from file <project-dir>/project and stored in Ctx
get_columns(Ctx, issue) ->
    Columns = proplists:get_value(issue_columns, Ctx#project.structure),
    % add "hidden" columns id, ctime, author  
    get_columns_automatic() ++ get_ordered_keys(Columns, []).

get_columns_automatic() -> [id, ctime, mtime, author].

%% Return the ordered list of keys of the given proplist.
%% This is not available from the proplists module
%% as proplists:get_keys/1 returns an unordered list.
get_ordered_keys([], Acc) -> lists:reverse(Acc);
get_ordered_keys([Atom | Rest], Acc) when is_atom(Atom) ->
    get_ordered_keys(Rest, [Atom | Acc]);    
get_ordered_keys([{Atom, _Value} | Rest], Acc) when is_atom(Atom) ->
    get_ordered_keys(Rest, [Atom | Acc]);
get_ordered_keys([_X | Rest], Acc) -> % ignore other cases
    get_ordered_keys(Rest, Acc).

get_value(Ctx, issue, id, I) ->
    Columns = get_columns(Ctx, issue),
    [issue | Values] = tuple_to_list(I),
    get_value(id, Columns, Values).

get_value(_Atom, _Columns = [], _Values = []) -> undefined;
get_value(Atom, [Atom | _Other_columns], [Value | _Other_values]) -> Value;
get_value(Atom, [_Col | Other_columns], [_Value | Other_values]) ->
    get_value(Atom, Other_columns, Other_values).


%% Make a diff of what changed between old and new issue
%% result is a proplists:
%% [{Key, {Old, New}}]
make_diff([], [], Acc) -> lists:reverse(Acc);
make_diff([Old_value | Rest_old], [New_value | Rest_new], Acc) ->
    case Old_value == New_value of
        true -> % no change
            make_diff(Rest_old, Rest_new, Acc);
        _Else -> % there is a change
            make_diff(Rest_old, Rest_new, [{Old_value,  New_value} | Acc])
    end.


