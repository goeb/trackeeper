%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Manage TKE database.
%%%

-module(tke_db).
-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([get/3]).

%% API -------------------

% start as many processes as there are projects in the directory
start() ->
    % TODO for now hardcoded project "tke"
    gen_server:start_link({local, tke}, tke_db, "tke", []).
    
get(Project, issue, N) ->
    gen_server:call(Project, {get, issue, N});
get(Project, message, N) ->
    gen_server:call(Project, {get, message, N}).

% if new issue, then create and return id.
% if existing issue, then update
update(Project, issue, Issue) ->
    gen_server:call(Project, {update, Issue}).

search(Project, Search) ->
    gen_server:call(Project, {search, Search}).

%% Internals -------------------

%% Project = list(char)
init(Project) ->
    log:debug("init(~p)", [Project]),
    % TODO load from disk and populate an ets table
    {ok, []}.

handle_call({get, issue, N}, _From, Chs) ->
    % TODO return the issue
    {reply, {issue, N}, Chs};
handle_call({get, message, N}, _From, Chs) ->
    {reply, {message, N}, Chs};
handle_call({update, Issue}, _From, Chs) ->
    {reply, {ok, 333}, Chs};
handle_call({search, Search}, _From, Chs) ->
    {reply, [], Chs}.

handle_cast(_X, Y) ->
    {noreply, Y}.

%% File access functions
load(Project) ->
    Issue_table = ets:new(issue,[private]),
    Message_table = ets:new(message,[private]),
    load_issues(Project, Issue_table),
    load_messages(Project, Message_table),
    {Issue_table, Message_table}.

load_issues(Project, Issue_table) ->
    {ok, Files} = file:list_dir(Project),
    Paths = [Project ++ "/" ++ File || File <- Files],
    load_issues_from_files(Files, Issue_table).

load_issues_from_files([], _Issue_table) -> ok;
load_issues_from_files([File | Others], Issue_table) ->
    {ok, Binary} = file:read_file(),
    S = binary_to_list(Binary),
    {ok, Tokens} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    ets:insert(Issue_table, Term),
    load_issues_from_files(Others, Issue_table).


load_messages(Project, Message_table) -> todo.
