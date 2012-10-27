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
%%% tke_db:update(tke, M#message{issue=1}).
%%%     {ok,#message{id = 4,issue = 1,author = undefined,
%%%          ctime = undefined,contents = undefined}}



-module(tke_db).
-behaviour(gen_server).

-export([start/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/2, search/3]).

-include("tke_db.hrl").
-record(project, {name, issues, messages}).
%% contents is either: {file, Filename} | {text, Text} | {change, [{Field, Old, New]}

%% API -------------------

% start as many processes as there are projects in the directory
start(Project) ->
    % TODO for now hardcoded project "tke"
    gen_server:start_link({local, list_to_atom(Project)}, tke_db, Project, []).

stop() -> gen_server:cast(tke, stop).
    
%% Project = atom() | list()
%% Table = issue | message
get(Project, Table, N) when is_list(Project) ->
    gen_server:call(list_to_atom(Project), {get, Table, N}).

% if new issue, then create and return id.
% if existing issue, then update
update(Project, Item) ->
    gen_server:call(Project, {update, Item}).

search(Project, Table, Search) ->
    gen_server:call(list_to_atom(Project), {search, Table, Search}).

%% Internals -------------------

%% Project = list(char)
init(Project) ->
    log:debug("init(~p)", [Project]),
    % TODO load from disk and populate an ets table
    {Issues, Messages} = load(Project),
    {ok, #project{name=Project, issues=Issues, messages=Messages}}.

handle_call({get, issue, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.issues, N) of
        [I0] -> I = convert_to_proplist(I0);
        [] -> I = undefined
    end,
    {reply, I, Ctx};
handle_call({get, message, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.messages, N) of
        [M0] -> log:debug("found message ~p", [M0]),
            M = convert_to_proplist(M0);
        [] -> M = undefined
    end,
    {reply, M, Ctx};
%% create new issue
handle_call({update, I = #issue{id=undefined}}, _From, Ctx) ->
    New_id = get_new_id(Ctx#project.issues),
    I2 = I#issue{id=New_id},
    handle_call({update, I2}, _From, Ctx);

%% update new issue with id, or existing issue (with id also)
handle_call({update, I = #issue{}}, _From, Ctx) ->
    ets:insert(Ctx#project.issues, I),
    log:debug("going to sync..."),
    sync(Ctx#project.name, I),
    {reply, {ok, I}, Ctx};
%% create new message
handle_call({update, M = #message{}}, _From, Ctx) ->
    New_id = get_new_id(Ctx#project.messages),
    M2 = M#message{id=New_id},
    ets:insert(Ctx#project.messages, M2),
    sync(Ctx#project.name, M2),
    {reply, {ok, M2}, Ctx};

handle_call({search, issue, _I}, _From, Ctx) ->
    {reply, [], Ctx};
%% Return messages that belong to the given issue id
handle_call({search, message, Issue_id}, _From, Ctx) ->
    Pattern = #message{issue=Issue_id, _ = '_'},
    log:debug("search ~p", [Pattern]),
    Messages = ets:match_object(Ctx#project.messages, Pattern),
    Mlist = [convert_to_proplist(M) || M <- Messages],
    {reply, Mlist, Ctx}.


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
            ets:insert(Issue_table, Term),
            log:debug("Issue ~p loaded.", [File])
    end,
    load_messages(Dir, Message_table),
    load_issues_from_dirs(Others, Issue_table, Message_table).

decode_contents(Binary) ->
    S = binary_to_list(Binary),
    {ok, Tokens, _EndLocation} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

load_messages(Dir, Message_table) -> 
    log:debug("load_messages(~p)", [Dir]),
    case file:list_dir(Dir) of
        {ok, Files} -> load_messages_from_files(Dir, Files, Message_table);
        {error, Reason} -> ok
    end.

load_messages_from_files(_Dir, [], _Message_table) -> ok;
% Consider files starting with "msg."
load_messages_from_files(Dir, [[$m, $s, $g, $. | Id]|Others], Message_table) ->
    File = Dir ++ "/msg." ++ Id,
    log:debug("Message ~p", [File]),
    case file:read_file(File) of
        {error, _Reason} -> log:debug("Message ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            ets:insert(Message_table, Term),
            log:debug("Message ~p loaded.", [File])
    end,
    load_messages_from_files(Dir, Others, Message_table);
% other files, not starting with "msg."
load_messages_from_files(Dir, [_F|Others], Message_table) ->
    log:debug("Message ~p (2)", [_F]),
    load_messages_from_files(Dir, Others, Message_table).


%% write to disk what has been modified
sync(Project, I = #issue{}) -> 
    log:debug("syncing..."),
    Id = integer_to_list(I#issue.id),
    Dirname = Project ++ "/" ++ Id,
    file:make_dir(Dirname),
    Filename = Dirname ++ "/issue",
    sync_file(Filename, I);

sync(Project, M = #message{}) ->
    log:debug("syncing message: ~p", [M]),
    Issue = integer_to_list(M#message.issue),
    Id = integer_to_list(M#message.id),
    Dirname = Project ++ "/" ++ Issue,
    Filename = Dirname ++ "/" ++ Id ++ ".msg",
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

convert_to_proplist(I = #issue{}) ->
    lists:zip(record_info(fields, issue), tl(tuple_to_list(I)));
convert_to_proplist(M = #message{}) ->
    lists:zip(record_info(fields, message), tl(tuple_to_list(M))).




