-module(tke_appmod).

-include("../../../yaws/include/yaws_api.hrl").
-include("tke_db.hrl").
-compile(export_all).

%% init server
start() ->
    io:format("~p starting...\n", [ ?MODULE ]),
    tke_db:start("tke"),
    tke_db:start("simpleP"),
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
no_project_name() -> tke_html:resource_not_found(). % not found

% action when no ressource is given
no_resource(_Project_name) -> tke_html:resource_not_found().

list_issues(Project_name, Query_params) ->
    log:info("list_issues(~p, issues, list, query=~p)\n", [Project_name, Query_params]),
    Colspec = proplists:get_value(colspec, Query_params),
    case Colspec of
        undefined -> Colspec2 = all;
        _else -> Colspec2 = string:tokens(Colspec, "+")
    end,
    Issues = tke_base:list(Project_name, [{columns, Colspec2}]),
    %Table = 
    [tke_html:header(Project_name),
     tke_html:format_table_of_issues(Issues),
     tke_html:footer(Project_name)
    ].

% show page for issue N
% N = list(char)
show_issue(Project, N, _Query) ->
    log:debug("show_issue(~p, ~p, ~p)", [Project, N, _Query]),
    Issue_id = list_to_integer(N),
    I = tke_db:get(Project, issue, Issue_id),
    Id = proplists:get_value(id, I),
    M = tke_db:search(Project, message, Id),
    case I of
        undefined -> Html = no_resource(Project);
        I -> Html = tke_html:show_issue(Project, I, M)
    end,

    % TODO
    %Issue = [{title, "critical error should be simple error"},
    %         {status, "open"},
    %         {owner,"John Smith"},
    %         {date, 123456}
    %        ],
    %log:debug("show_issue: Html=~p", [Html]),
    Html.


% make a proplist from the query string
% x=111&y=bbb
% -> [{x, "111"}, {y, "bbb"}]
parse_query_string(undefined) -> [];
parse_query_string(Query_string) ->
    log:debug("Query_string=~p", [Query_string]),
    Q = string:tokens(Query_string, "&"),
    Q2 = [ string:tokens(X, "=") || X <- Q],
    [{list_to_atom(X), Y} || [X, Y] <- Q2]. % make a proplist


serve('GET', Url_tokens, Query_string) -> http_get(Url_tokens, Query_string);
serve('POST', Url_tokens, Query_string) -> http_post(Url_tokens, Query_string).

http_post(_Url_tokens, _Query_string) -> tke_html:resource_not_found().

% HTTP get
% Project = name of project = list(char)
% Resource = "issue" | TODO
% Query = proplists of items of the HTTP query string
http_get([], _Query) -> tke_html:resource_not_found();
http_get([Project], _Query) -> no_resource(Project);
http_get([Project, "issue"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", "list"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", N], Query) -> show_issue(Project, N, Query);
http_get(A, B) -> log:info("Invalid GET request ~p ? ~p", [A, B]),
    tke_html:resource_not_found().

out(A) ->
    %log:debug("out(~p)", [A]),
    Method = A#arg.req#http_request.method,
    Q = parse_query_string(A#arg.querydata),
    % TODO parse cookie / get session
    Url_tokens = string:tokens(A#arg.appmoddata, "/"),
    serve(Method, Url_tokens, Q).

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


