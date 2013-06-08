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
-export([user_login/2]).

%% API -------------------

start() ->
    gen_server:start_link({local, registered_name()},
                          tke_user, [], []).

stop() -> gen_server:cast(registered_name(), stop).

%% Get name of user that associated with a cookie
%% Return: list(char) | undefined
get_user(Session_id) -> 
    gen_server:call(registered_name(), {get_user, Session_id}).

%% Get the list of users of a given project
%% Return: ["UserName"]
get_list_of_users(Project) ->
    gen_server:call(registered_name(), {get_list_of_users, Project}).    

%% Check if user is allowed to log in, and if so, then log him/her in.
%% Returns: {ok, randomId} or error
user_login(Username, Password) ->
    gen_server:call(registered_name(), {user_login, Username, Password}).


%% Internals -------------------

registered_name() -> tke_user.
    
init(_) ->
    log:debug("init tke_user"),
    Config = load("users.conf"),
    Logged_in = [],
    {ok, {Config, Logged_in}}.

handle_call({get_user, Session_id}, _From, Ctx={Config, Logged_users}) ->
    R = proplists:get_value(Session_id, Logged_users),
    {reply, R, Ctx};

handle_call({get_list_of_users, Project}, _From, Ctx={Config, Logged_users}) ->
    R = [ Username || {Username, _} <- Config],
    {reply, R, Ctx};

handle_call({user_login, Username, Password}, _From, Ctx={Cfg, Logged_users}) ->
    Userinfo = proplists:get_value(Username, Cfg),
    log:debug("Userinfo=~p", [Userinfo]),
    case Userinfo of
        undefined ->
            R = {error, unknown_username},
            Newctx = Ctx;
        Userinfo ->
            Expected_sha1 = proplists:get_value(sha1, Userinfo),
            R0 = check_sha1(Password, Expected_sha1),
            case R0 of
                ok ->
                    Sid0 = random:uniform(),
                    Sid1 = term_to_binary(Sid0),
                    Sid2 = crypto:sha(Sid1),
                    Sid3 = binary_to_hex_string(Sid2),
                    Logged_tmp = proplists:delete(Sid3, Logged_users),
                    Logged_new = [{Sid3, Username} | Logged_tmp],
                    Newctx = {Cfg, Logged_new},
                    R = {ok, Sid3};
                R ->
                    Newctx = Ctx
            end
    end,
    {reply, R, Newctx}.

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

%% Expected_sha1_hex = string() in hexadecimal notation
%% Password = string()
check_sha1(Password, Expected_sha1_hex) ->
    Provided_sha1 = crypto:sha(Password),
    Provided_sha1_hex = binary_to_hex_string(Provided_sha1),
    case Provided_sha1_hex of 
        Expected_sha1_hex -> ok;
        _Else -> {error, invalid_password}
    end.

binary_to_hex_string(Bin) ->
    [ hd(erlang:integer_to_list(Nib, 16)) || << Nib:4 >> <= Bin ].
