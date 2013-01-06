-module(tke_appmod).

-include("../../../yaws/include/yaws_api.hrl").
-include("tke_db.hrl").
-compile(export_all).

%% init server
start() ->
    io:format("~p starting...\n", [ ?MODULE ]),
    tke_db:start("tke"), % TODO start as many tke_db as there are projects
    tke_user:start(),
    %tke_db:start("simpleP"),
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
    Colspec = get_colspec(Query_params),
    Sorting = get_sorting(Query_params),
    Filter = get_filter(Query_params),
    {Columns, Issues} = tke_db:search(Project_name, issue,
        [{columns, Colspec}, {sort, Sorting}]),
    [tke_html:header(Project_name),
     tke_html:format_table_of_issues(Columns, Issues),
     tke_html:footer(Project_name)
    ].

get_colspec(Query_params) ->
    Colspec = proplists:get_value(colspec, Query_params),
    case Colspec of
        undefined -> all;
        _Else -> [list_to_atom(X) || X <- string:tokens(Colspec, "+")]
    end.

%% sort=id+title-owner
%% => id ascending, then title ascending, then owner descending
get_sorting(Query_params) ->
    Sorting = proplists:get_value(sort, Query_params),
    case Sorting of
        undefined -> undefined;
        _Else -> [list_to_atom(X) || X <- string:tokens(Sorting, "+")]
    end.

%% filter=label:v1.0+status:open
%% => [{label, "v1.0"}, {status, "open"}]
get_filter(Query_params) ->
    Filter = proplists:get_value(filter, Query_params),
    parse_filter(Filter).

parse_filter(undefined) -> undefined;
parse_filter(Filter) ->
    Groups = string:tokens(Filter, "+"),
    [string:tokens(X, ":") || X <- Groups]
    .




% show page for issue N
% N = list(char)
show_issue(Project, N, _Query) ->
    log:debug("show_issue(~p, ~p, ~p)", [Project, N, _Query]),
    Issue_id = list_to_integer(N),
    I = tke_db:get(Project, issue, Issue_id),
    Id = proplists:get_value(id, I),
    M = tke_db:search(Project, message, Id),
    H = tke_db:search(Project, history, Id),
    case I of
        undefined -> Html = no_resource(Project);
        I -> Html = tke_html:show_issue(Project, I, M, H)
    end,
    %log:debug("Html=~p", [Html]),
    Html.

new_issue(Project, _Query) ->
    tke_html:show_issue(Project, tke_db:get(Project, issue, empty), [], []).


% make a proplist from the query string
% x=111&y=bbb
% -> [{x, "111"}, {y, "bbb"}]
parse_query_string(undefined) -> [];
parse_query_string(Query_string) ->
    log:debug("Query_string=~p", [Query_string]),
    Q = string:tokens(Query_string, "&"),
    Q2 = [ string:tokens(X, "=") || X <- Q],
    [{list_to_atom(X), Y} || [X, Y] <- Q2]. % make a proplist


serve('GET', Url_tokens, Http_req) -> 
    Q = parse_query_string(Http_req#arg.querydata),
    http_get(Url_tokens, Q);

serve('POST', Url_tokens, Http_req) ->
    log:debug("Http_req=~p", [Http_req]),
    log:debug("content_type=~p", [Http_req#arg.headers#headers.content_type]),

    case string:str(Http_req#arg.headers#headers.content_type, "multipart/form-data") of
        0 -> % not found. This is not a "multipart/form-data"
            Data = yaws_api:parse_post(Http_req),
            http_post(Url_tokens, Data);
        _Else ->
            case yaws_api:parse_multipart_post(Http_req) of
                {result, Data} -> 
                        % consolidate Multipart_data
                        Data_proplist = consolidate_multipart(Data, []),
                        log:debug("Data_proplist=~p", [Data_proplist]),
                        http_post(Url_tokens, Data_proplist);
                {cont, Cont, _Res} ->
                    % TODO accumulate data
                    {get_more, Cont, todo}
            end
    end.
    %Post_data = yaws_api:parse_post(Http_req),
    %log:debug("Post: Multipart=~p", [Multipart]),
    %log:debug("Post: Post_data=~p", [Post_data]),

http_post([Project, "issue", N], Issue) -> 
    I2 = proplists:delete(id, Issue), % normally not needed
    case N of
        "new" -> % set id = undefined
            I3 = [{id, undefined} | I2];
        N -> 
            Id = list_to_integer(N),
            I3 = [{id, Id} | I2]
    end,
    {ok, Id4} = tke_db:update(Project, issue, I3),
    log:debug("Id4=~p", [Id4]),
    case N of
        "new" ->
            Redirect_url = "/" ++ Project ++ "/issue/" ++ integer_to_list(Id4),
            log:debug("redirect to: " ++ Redirect_url),
            {redirect, Redirect_url};
        N -> show_issue(Project, N, "")
    end;


http_post(["login"], Login) -> 
    log:debug("Login data: ~p", [Login]),
    % TODO handle login
    tke_html:login_page().

% convert multipart list to a proplist
consolidate_multipart([], Acc) -> 
    log:debug("consolidate_multipart: result=~p", [Acc]),
    Acc;
consolidate_multipart([{head,{Name,_Headers}}, {body, Value} | Others], Acc) ->
    Atom = list_to_atom(Name), % TODO prevent DOS via too many atoms
    % in case of multiple select, we get: {tags,"v1.0"}, {tags,"v1.1"}, etc.
    % and we need: {tags, ["v1.0", "v1.1",...]}
    case proplists:get_value(Atom, Acc) of
        undefined -> 
            log:debug("consolidate_multipart: undefined ~p", [Atom]),
            Acc2 = [{Atom, Value}|Acc];

        Old_value ->
            log:debug("consolidate_multipart: Old_value=~p", [Old_value]),
            % if list of char "v1.0", then it must be enclosed in new list
            % if not, then it is already ["v1.0", "v1.1"]
            Acc1 = proplists:delete(Atom, Acc),

            log:debug("consolidate_multipart: Acc1=~p", [Acc1]),
            [X | _Y] = Old_value,
            case X of
                X when is_integer(X) -> % case of a string (list of char)
                    Acc2 = [{Atom, [Value, Old_value]}|Acc1];
                X ->
                    Acc2 = [{Atom, [Value | Old_value]}|Acc1]
        end
    end,
    consolidate_multipart(Others, Acc2).


% HTTP get
% Project = name of project = list(char)
% Resource = "issue" | TODO
% Query = proplists of items of the HTTP query string
http_get([], _Query) -> tke_html:resource_not_found();
http_get(["login"], _Query) -> tke_html:login_page();
http_get([Project], _Query) -> no_resource(Project);
http_get([Project, "issue"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", "list"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", "new"], Query) -> new_issue(Project, Query);
http_get([Project, "issue", N], Query) -> show_issue(Project, N, Query);
http_get(A, B) -> log:info("Invalid GET request ~p ? ~p", [A, B]),
    tke_html:resource_not_found().

out(A) ->
    %log:debug("out(~p)", [A]),
    Method = A#arg.req#http_request.method,
    % TODO parse cookie / get session
    Url_tokens = string:tokens(A#arg.appmoddata, "/"),
    serve(Method, Url_tokens, A).

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


