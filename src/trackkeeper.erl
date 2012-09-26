-module(fhappmod).

-include("../../../yaws/include/yaws_api.hrl").
-compile(export_all).

%% init server
start() ->
    io:format("fhappmod starting...\n"),
    spawn(?MODULE, loop, []),
    ok.

loop() ->
    ets:new(fhtable, [set, public, named_table]),
    receive
      _ -> ok
    end.

getHistory(Id) ->
    R = ets:lookup(fhtable, Id),
    case R of
      [] -> [];
      [{_Cookie, Data}] -> Data
    end.

into(Cookie, Data) ->
    ets:insert(fhtable, {Cookie, Data}).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

new_session() ->
    M = 333,
    Cookie = yaws_api:new_cookie_session(M),
    CO = yaws_api:setcookie("id",Cookie,"/"),
    { CO, M }
    .


out(A) ->
    H = A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("id", C) of
        [] ->
            { CO, Id } = new_session(),
            PreviousHistory = [];
        Cookie ->
            case yaws_api:cookieval_to_opaque(Cookie) of
                {ok, Id} ->
                    CO = none,
                    PreviousHistory = getHistory(Id);
                {error, no_session} ->
                    { CO, Id } = new_session(),
                    PreviousHistory = [ no_session ]
            end
    end,

        
    NewHistory = [ A#arg.appmoddata | PreviousHistory ],
    into(Id, NewHistory),
    
    Html = {ehtml, [{p,[],
       box(io_lib:format("A#arg.appmoddata = ~p~n"
                         "A#arg.appmod_prepath = ~p~n"
                         "A#arg.querydata = ~p~n"
                         "history = ~p\n",
                         [A#arg.appmoddata,
                          A#arg.appmod_prepath,
                          A#arg.querydata,
                          NewHistory]))}]},
    case CO of 
        none -> Html;
        _ -> [ Html, CO ]
    end.



