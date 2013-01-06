%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%% Manage TKE users
%%%

-module(tke_user).
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get_user/1, get_list_of_users/1]).
-export([check_user_login/2]).

%% API -------------------

start() ->
    gen_server:start_link({local, registered_name()},
                          tke_user, [], []).

stop() -> gen_server:cast(registered_name(), stop).

%% Get name of user that associated with a cookie
%% Return: {ok, "UserName"} | {error, not_logged_in}
get_user(Cookie) -> 
    gen_server:call(registered_name(), {get_user, Cookie}).    

%% Get the list of users of a given project
%% Return: ["UserName"]
get_list_of_users(Project) ->
    gen_server:call(registered_name(), {get_list_of_users, Project}).    

%% Check if user is allowed to log in.
%% Returns: {ok, randomId} or error
check_user_login(Username, Password) ->
    gen_server:call(registered_name(), {check_user_login, Username, Password}).


%% Internals -------------------

registered_name() -> tke_user.
    
init(_) ->
    log:debug("init tke_user"),
    Config = load("users.conf"),
    Logged_in = [],
    {ok, {Config, Logged_in}}.

handle_call({get_user, Cookie}, _From, Ctx) ->
    R = "Mike",
    % TODO
    {reply, R, Ctx};

handle_call({get_list_of_users, Project}, _From, Ctx) ->
    R = ["John", "Mike", "Anna", "Alice", "Fred"],
    {reply, R, Ctx};

handle_call({check_user_login, Username, Password}, _From, Ctx) ->
    R = {ok, 1234},
    {reply, R, Ctx}.

handle_cast(stop, Ctx) -> {stop, normal, Ctx};
handle_cast(_X, Y) -> {noreply, Y}.

%% File access functions
load(File) ->
    {ok, Binary} = file:read_file(File),
    Term = decode_contents(Binary),
    Term.

decode_contents(Binary) ->
    S = binary_to_list(Binary),
    {ok, Tokens, _EndLocation} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

code_change(_, _, _) -> ok.
handle_info(_, _) -> ok.
terminate(shutdown, _State) -> ok.

