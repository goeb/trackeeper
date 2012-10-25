%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Manage TKE database.
%%%

-module(tke_db).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/2]).

-record(project, {name, issues, messages}).
-record(issue, {id, title, status, owner, summary, ctime, tags}).
-record(message, {id, author, ctime, contents}).
%% contents is either: {file, Filename} | {text, Text} | {update, Text}

%% API -------------------

% start as many processes as there are projects in the directory
start() ->
    % TODO for now hardcoded project "tke"
    gen_server:start_link({local, tke}, tke_db, "tke", []).

stop() -> gen_server:cast(tke, stop).
    
%% Project = atom()
get(Project, issue, N) ->
    gen_server:call(Project, {get, issue, N});
get(Project, message, N) ->
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
        [I] -> ok;
        [] -> I = none
    end,
    {reply, I, Ctx};
handle_call({get, message, N}, _From, Ctx) ->
    {reply, {message, N}, Ctx};
handle_call({update, I = #issue{id=new}}, _From, Ctx) ->
    New_id = get_new_id(Ctx#project.issues),
    I2 = I#issue{id=New_id},
    ets:insert(Ctx#project.issues, I2),
    log:debug("going to sync..."),
    sync(Ctx#project.name, I2),
    {reply, {ok, I2}, Ctx};
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
    Filename = Project ++ "/" ++ Id ++ "/issue",
        %% TODO create dir if needed
    Str = io_lib:format("~p.", [I]),
    Bytes = list_to_binary(Str),
    X=file:write_file(Filename, Bytes),
    log:debug("syncing...~p, Filename=~p", [X, Filename]),
    X;
sync(Project, I = #message{}) -> ok.

code_change(_, _, _) -> ok.
handle_info(_, _) -> ok.
terminate(shutdown, _State) -> ok.

%% Get new id functions
get_new_id(Issues) -> get_max_id(Issues, ets:first(Issues), 0) + 1.

get_max_id(Issues, '$end_of_table', N) -> N;
get_max_id(Issues, Key, N) ->
    case Key > N of
        true -> Max = Key;
        _Else -> Max = N
    end,
    get_max_id(Issues, ets:next(Issues, Key), Max).

