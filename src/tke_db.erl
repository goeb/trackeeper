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
-export([get/3]).

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
update(Project, issue, Issue) ->
    gen_server:call(Project, {update, Issue}).

search(Project, Search) ->
    gen_server:call(Project, {search, Search}).

%% Internals -------------------

%% Project = list(char)
init(Project) ->
    log:debug("init(~p)", [Project]),
    % TODO load from disk and populate an ets table
    {Issues, Messages} = load(Project),
    {ok, {Issues, Messages}}.

handle_call({get, issue, N}, _From, {Issues, _Messages}) ->
    case ets:lookup(Issues, N) of
        [I] -> ok;
        [] -> I = none
    end,
    {reply, I, {Issues, _Messages}};
handle_call({get, message, N}, _From, Chs) ->
    {reply, {message, N}, Chs};
handle_call({update, _Issue}, _From, Chs) ->
    {reply, {ok, 333}, Chs};
handle_call({search, _Search}, _From, Chs) ->
    {reply, [], Chs}.

handle_cast(stop, State) -> {stop, normal, State};
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

code_change(_, _, _) -> ok.
handle_info(_, _) -> ok.
terminate(shutdown, _State) -> ok.
