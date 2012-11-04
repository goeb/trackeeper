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

-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/3, search/3, get_empty_issue/1]).

-include("tke_db.hrl").
-record(project, {name, issues, messages}).
%% contents is either: {file, Filename} | {text, Text} | {change, [{Field, Old, New]}

%% API -------------------

% start as many processes as there are projects in the directory
start(Project) ->
    gen_server:start_link({local, list_to_atom(Project)}, tke_db, Project, []).

stop() -> gen_server:cast(tke, stop).
    
%% Project = atom() | list()
%% Table = issue | message
get(Project, Table, N) when is_list(Project) ->
    gen_server:call(list_to_atom(Project), {get, Table, N}).

%% if new issue, then create and return id.
%% if existing issue, then update
%% Issue contains the message
update(Project, issue, Issue) ->
    log:debug("update0: "),
    gen_server:call(list_to_atom(Project), {update, Issue}).

search(Project, Table, Search) ->
    gen_server:call(list_to_atom(Project), {search, Table, Search}).

%% Return proplists of issue, with all fields undefined
get_empty_issue(Project) ->
    Cols = get_columns(Project, issue),
    Values = lists:duplicate(length(Cols), undefined),
    I = list_to_tuple([issue | Values]), 
    convert_to_proplist(I).


%% Internals -------------------

%% Project = list(char)
init(Project) ->
    log:debug("Loading project ~p", [Project]),
    % TODO load from disk and populate an ets table
    {Issues, Messages} = load(Project),
    log:debug("Loading project ~p completed", [Project]),
    {ok, #project{name=Project, issues=Issues, messages=Messages}}.

handle_call({get, issue, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.issues, N) of
        [I0] -> I = convert_to_proplist(I0);
        [] -> I = undefined
    end,
    {reply, I, Ctx};
handle_call({get, message, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.messages, N) of
        [M0] -> %log:debug("found message ~p", [M0]),
            M = convert_to_proplist(M0);
        [] -> M = undefined
    end,
    {reply, M, Ctx};
%% create new issue
%% Issue contains the message
handle_call({update, Issue}, _From, Ctx) ->
    log:debug("update: Issue=~p", [Issue]),
    Id0 = proplists:get_value(id, Issue),
    % TODO the diff of the issue wrt. to previous value
    case Id0 of 
        undefined ->
            Id = get_new_id(Ctx#project.issues),
            Issue2 = proplists:delete(id, Issue), % replace id
            Issue3 = [{id, Id} | Issue2];
        Id0 ->
            Id = Id0,
            Issue3 = Issue
    end,
    % add
    I = convert_to_entry(Ctx#project.name, issue, Issue3),
    ets:insert(Ctx#project.issues, I),
    log:debug("going to sync..."),
    sync(Ctx#project.name, I),
    % now add the message
    add_message(Id, Issue, Ctx),
    {reply, {ok, Id}, Ctx};
%% create new message
handle_call({update, message, Message}, _From, Ctx) ->
    M = convert_to_record(message, Message),
    New_id = get_new_id(Ctx#project.messages),
    M2 = M#message{id=New_id},
    ets:insert(Ctx#project.messages, M2),
    sync(Ctx#project.name, M2),
    {reply, {ok, M2}, Ctx};

% Search : proplist
%   key 'columns' : list of columns needed in the return value
%   key 'pattern' : pattern for selective search TODO
handle_call({search, issue, Search}, _From, Ctx) ->
    log:debug("search issue: Search=~p", [Search]),
    % for now, return the list of all issues
    % TODO filter
    Pattern_l = lists:duplicate(length(get_columns(Ctx#project.name, issue)), '_'),
    Pattern = list_to_tuple([issue | Pattern_l]),
    Issues = ets:match_object(Ctx#project.issues, Pattern),
    I_list = [convert_to_proplist(I) || I <- Issues],
    % now keep only the needed columns
    Columns = proplists:get_value(columns, Search),
    case Columns of
        all -> Needed_columns = get_columns(Ctx#project.name, issue);
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
    Mlist = [convert_to_proplist(M) || M <- Messages],
    {reply, Mlist, Ctx}.

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
    Issue_table = ets:new(issue,[private, {keypos, 2}]),
    Message_table = ets:new(message,[private, {keypos, 2}]),
    load_issues(Project, Issue_table, Message_table),
    {Issue_table, Message_table}.

load_issues(Project, Issue_table, Message_table) ->
    {ok, Files} = file:list_dir(Project),
    Dirs = [Project ++ "/" ++ File || File <- Files],
    load_issues_from_dirs(Dirs, Issue_table, Message_table).

load_issues_from_dirs([], _Issue_table, _Message_table) -> ok;
load_issues_from_dirs([Dir | Others], Issue_table, Message_table) ->
    File = Dir ++ "/issue",
    case file:read_file(File) of
        {error, _Reason} -> ok;
        {ok, Binary} ->
            Term = decode_contents(Binary),
            ets:insert(Issue_table, Term)
            %log:debug("Issue ~p loaded.", [File])
    end,
    load_messages(Dir, Message_table),
    load_issues_from_dirs(Others, Issue_table, Message_table).

decode_contents(Binary) ->
    S = binary_to_list(Binary),
    {ok, Tokens, _EndLocation} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

load_messages(Dir, Message_table) -> 
    %log:debug("load_messages(~p)", [Dir]),
    case file:list_dir(Dir) of
        {ok, Files} -> load_messages_from_files(Dir, Files, Message_table);
        {error, _Reason} -> ok
    end.

load_messages_from_files(_Dir, [], _Message_table) -> ok;
% Consider files starting with "msg."
load_messages_from_files(Dir, [[$m, $s, $g, $. | Id]|Others], Message_table) ->
    File = Dir ++ "/msg." ++ Id,
    %log:debug("Message ~p", [File]),
    case file:read_file(File) of
        {error, _Reason} -> log:debug("Message ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            case merge_to_record(message, Term) of
                {error, Reason} -> log:error("Cannot load message ~p: ~p",
                                             [Id, Reason]);
                {ok, T} -> ets:insert(Message_table, T)
            end
            % TODO if Term != T, the sync on th disk
            % (record structure has changed)
            %log:debug("Message ~p loaded.", [File])
    end,
    load_messages_from_files(Dir, Others, Message_table);
% other files, not starting with "msg."
load_messages_from_files(Dir, [_F|Others], Message_table) ->
    %log:debug("Message ~p (2)", [_F]),
    load_messages_from_files(Dir, Others, Message_table).


%% write to disk what has been modified
sync(Project, I) when element(1, I) == issue -> 
    log:debug("syncing..."),
    Id = integer_to_list(get_value(Project, issue, id, I)),
    Dirname = Project ++ "/" ++ Id,
    file:make_dir(Dirname),
    Filename = Dirname ++ "/issue",
    sync_file(Filename, I);

sync(Project, M = #message{}) ->
    log:debug("syncing message: ~p", [M]),
    Issue = integer_to_list(M#message.issue),
    Id = integer_to_list(M#message.id),
    Dirname = Project ++ "/" ++ Issue,
    Filename = Dirname ++ "/msg." ++ Id,
    sync_file(Filename, M).

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

convert_to_proplist(I) when element(1, I) == issue ->
    lists:zip(get_columns(xxx, issue), tl(tuple_to_list(I)));
convert_to_proplist(M = #message{}) ->
    lists:zip(record_info(fields, message), tl(tuple_to_list(M))).

% Message or Issue = proplist()
convert_to_record(message, M) ->
    Keys = record_info(fields, message),
    convert_to_record(Keys, M, [message]).

convert_to_record([], _Proplist, Record) ->
    list_to_tuple(lists:reverse(Record));
convert_to_record([Key | Others], Proplist, Record) ->
    Value = proplists:get_value(Key, Proplist),
    convert_to_record(Others, Proplist, [Value|Record]).

%% Project = list(char)  = Name of project
%% I       = proplists() = Issue to be converted
convert_to_entry(Project, issue, I) ->
    Keys = get_columns(Project, issue),
    convert_to_record(Keys, I, [issue]).

sort(I_list, undefined) -> I_list;
%% Sort = list(atom())
sort(I_list, [Col]) ->
    % less than or equal function
    Lte = fun(A, B) ->
            proplists:get_value(Col, A) < proplists:get_value(Col, B) end,
    lists:sort(Lte, I_list);
   
%% TODO multi-column  sorting
sort(_I_list, _Sort) -> todo.


%% Issue_id : id of related issue
%% Message = proplists() (contains also Issue info, but not needed here)
%% Ctx : context of the server    
add_message(Issue_id, Message, Ctx) ->
    Text = proplists:get_value(message, Message),
    %% TODO File = proplists:get_value(file, Message),
    TS = os:timestamp(),
    Timestamp = calendar:now_to_universal_time(TS),
    % TODO author
    Id = get_new_id(Ctx#project.messages),
    M = #message{id=Id, issue=Issue_id, author="John Doe", ctime=Timestamp,
             text=Text},

    ets:insert(Ctx#project.messages, M),
    sync(Ctx#project.name, M),
    ok.

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
%% Columns are read from file <project-dir>/project
get_columns(_Project, issue) ->
    [id, title, status, owner, summary, ctime, tags].


get_value(Project, issue, id, I) ->
    Columns = get_columns(Project, issue),
    [issue | Values] = tuple_to_list(I),
    get_value(id, Columns, Values).

get_value(_Atom, _Columns = [], _Values = []) -> undefined;
get_value(Atom, [Atom | _Other_columns], [Value | _Other_values]) -> Value;
get_value(Atom, [_Col | Other_columns], [_Value | Other_values]) ->
    get_value(Atom, Other_columns, Other_values).


