-module(tke_appmod).

-include("../../../yaws/include/yaws_api.hrl").
-compile(export_all).

%% init server
start() ->
    io:format("~p starting...\n", [ ?MODULE ]),
    spawn(?MODULE, loop, []),
    ok.

loop() ->
    ets:new(tke_sessions, [set, public, named_table]),
    receive
      _ -> ok
    end.

getHistory(Id) ->
    R = ets:lookup(tke_sessions, Id),
    case R of
      [] -> [];
      [{_Cookie, Data}] -> Data
    end.

into(Cookie, Data) ->
    ets:insert(tke_sessions, {Cookie, Data}).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

new_session() ->
    M = 333,
    Cookie = yaws_api:new_cookie_session(M),
    CO = yaws_api:setcookie("id",Cookie,"/"),
    { CO, M }
    .

% action when no project name is given in the request
no_project_name() -> {status, 404}. % not found

% action when no ressource is given
no_resource(Project_name) ->
    { html, "project " ++ Project_name}.


serve_resource(Project_name, "issues", []) ->
    serve_resource(Project_name, "issues", ["list"]);
serve_resource(Project_name, "issues", ["list"]) ->
    log:info("serve_resource(~p, issues, list)\n", [Project_name]),
    Issues = tke_base:list(Project_name),
    %Table = 
    log:info("file:read_file(~p)", [Project_name ++ "/header.html"]),
    {ok, Header} = file:read_file(Project_name ++ "/header.html"),
    {ok, Footer} = file:read_file(Project_name ++ "/footer.html"),
    [   {html, Header},
        {ehtml, { pre, [], io_lib:format("~p\n", [Issues]) } },
        {ehtml, {table, [{class, "t_issues"}], [ {html, ["toto\n"]}, {html, ["titi\n"]}] } },
        tke_html:format_table_of_issues(Issues),
        {html, Footer}]
    ;
serve_resource(Project_name, Resource_name, Details) ->
    log:info("Project_name=~p, Resource=~p, Rest=~p\n",
        [ Project_name, Resource_name, Details ]),
    { html, "xxx" }.

out(A) ->
    Url_tokens = string:tokens(A#arg.appmoddata, "/"),
    case Url_tokens of
        [] -> no_project_name();
        [ Project_name ] -> no_resource(Project_name);
        [ Project_name, Resource | Details ] ->
            serve_resource(Project_name, Resource, Details)
    end.

    %Query_string = A#arg.querydata,
    %log:info("Query_string=~p\n", [ Query_string ]),
    %log:info("req=~p\n", [ A#arg.req ]),

    %% old code for managing session
out_old(A) ->
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


