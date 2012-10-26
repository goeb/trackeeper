%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Manage TKE database.
%%%
%%%
%%%
%%% Example:
%%% tke_db:get(tke, message, 4).
%%%     #message{id = 4,issue = 1,author = undefined,
%%%              ctime = undefined,contents = undefined}
%%% tke_db:get(tke, issue, 2).
%%%     ...
%%% tke_db:update(tke, M#message{issue=1}).
%%%     {ok,#message{id = 4,issue = 1,author = undefined,
%%%          ctime = undefined,contents = undefined}}



-module(tke_db).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/2]).

-include("tke_db.hrl").
-record(project, {name, issues, messages}).
%% contents is either: {file, Filename} | {text, Text} | {change, [{Field, Old, New]}

%% API -------------------

% start as many processes as there are projects in the directory
start() ->
    % TODO for now hardcoded project "tke"
    gen_server:start_link({local, tke}, tke_db, "tke", []).

stop() -> gen_server:cast(tke, stop).
    
%% Project = atom() | list()
get(Project, Kind, N) when is_list(Project) ->
    get(list_to_atom(Project), Kind, N);
get(Project, issue, N) when is_atom(Project) ->
    gen_server:call(Project, {get, issue, N});
get(Project, message, N) when is_atom(Project) ->
    gen_server:call(Project, {get, message, N}).

% if new issue, then create and return id.
% if existing issue, then update
update(Project, Item) ->
    gen_server:call(Project, {update, Item}).

search(Project, Search) ->
    gen_server:call(Project, {search, Search}).

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
        [M0] -> M = convert_to_proplist(M0);
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

handle_call({search, _Search}, _From, Ctx) ->
    {reply, [], Ctx}.

handle_cast(stop, Ctx) -> {stop, normal, Ctx};
handle_cast(_X, Y) -> {noreply, Y}.

%% File access functions
load(Project) ->
    Issue_table = ets:new(issue,[private, {keypos, 2}]),
    Message_table = ets:new(message,[private, {keypos, 2}]),
    load_issues(Project, Issue_table),
    load_messages(Project, Message_table),
    {Issue_table, Message_table}.

load_issues(Project, Issue_table) ->
    {ok, Files} = file:list_dir(Project),
    Paths = [Project ++ "/" ++ File ++ "/issue" || File <- Files],
    load_issues_from_files(Paths, Issue_table).

load_issues_from_files([], _Issue_table) -> ok;
load_issues_from_files([File | Others], Issue_table) ->
    case file:read_file(File) of
        {error, _Reason} -> ok,
            log:debug("Skip ~p", [File]);% skip this file
        {ok, Binary} ->
            S = binary_to_list(Binary),
            {ok, Tokens, _EndLocation} = erl_scan:string(S),
            {ok, Term} = erl_parse:parse_term(Tokens),
            ets:insert(Issue_table, Term),
            log:debug("Issue ~p loaded.", [File])
    end,
    load_issues_from_files(Others, Issue_table).


load_messages(_Project, _Message_table) -> todo.

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
    lists:zip(record_info(fields, issue), tl(tuple_to_list(I))).




