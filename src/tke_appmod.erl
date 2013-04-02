-module(tke_appmod).

-include("../../../yaws/include/yaws_api.hrl").
-include("tke_db.hrl").
-compile(export_all).

%% init server
start() ->
    io:format("~p starting...\n", [ ?MODULE ]),
    tke_db:start("tke"), % TODO start as many tke_db as there are projects
    tke_db:start("maison"), % TODO start as many tke_db as there are projects
    tke_user:start(),
    ok.

% action when no ressource is given
no_resource(_Project_name) -> tke_rendering_html:resource_not_found().

list_issues(Project_name, Query_params) ->
    log:info("list_issues(~p, issues, list, query=~p)\n", [Project_name, Query_params]),
    Colspec = get_colspec(Query_params),
    Sorting = get_sorting(Query_params),
    {Keep, Exclude} = get_filter(Query_params),
    Search_text = get_search_text(Query_params),
    Module = get_format(Query_params),
    {Columns, Issues} = tke_db:search(Project_name, issue,
        [{columns, Colspec}, {sort, Sorting}, {search, Search_text},
         {keep, Keep}, {exclude, Exclude}
        ]),
    log:debug("apply Module=~p", [Module]),
    apply(Module, list_issues, [Project_name, Columns, Issues]).

get_colspec(Query_params) ->
    Colspec = proplists:get_value("colspec", Query_params),
    case Colspec of
        undefined -> default;
        "all" -> all;
        _Else -> [list_to_atom(X) || X <- string:tokens(Colspec, "+")]
    end.

get_format(Query_params) ->
    log:debug("Q=~p", [Query_params]),
    Format = proplists:get_value("format", Query_params),
    case Format of
        "erlang" -> F = tke_rendering_erlang;
        "json" -> F = tke_rendering_json;
        _Default -> F = tke_rendering_html
    end,
    log:debug("Format=~p", [F]),
    F.


%% sort=id+title-owner
%% => id ascending, then title ascending, then owner descending
get_sorting(Query_params) ->
    Sorting = proplists:get_value("sort", Query_params),
    case Sorting of
        undefined -> undefined;
        _Else -> [list_to_atom(X) || X <- string:tokens(Sorting, "+")]
    end.

%% @spec get_filter(Query_params) -> {Keep, Exclude}
%%      Keep    = [{Key, Value}]
%%      Exclude = [{Key, Value}]
%%
%% filter=label:v1.0+owner:John Doo-status:closed-status:deleted
%% => {[{"label", "v1.0"}, {"owner", "John Doo"}], 
%%     [{"status", "closed"}, {"status", "deleted"}]
%%    }
get_filter(Query_params) ->
    Filter = proplists:get_value("filter", Query_params),
    parse_filter(Filter).

%% TODO for now only + (keep) are inmplemented)
parse_filter(undefined) -> {undefined, undefined}; % {Keep, Exclude}
parse_filter(Filter) ->
    Groups = string:tokens(Filter, "+"),
    Keep = [string:tokens(X, ":") || X <- Groups],
    Keep1 = [{Key, Value} || [Key, Value] <- Keep],
    {Keep1, []}. % TODO exclude not implemented

%% get the searched text from the query string
get_search_text(Query_params) ->
    Search_text = proplists:get_value("search", Query_params),
    Search_text.





% show page for issue N
% Issue_id = list(char)
show_issue(Project, Issue_id, Query) ->
    log:debug("show_issue(~p, ~p, ~p)", [Project, Issue_id, Query]),
    Module = get_format(Query),
    I = tke_db:get(Project, issue, Issue_id),
    case I of
        undefined -> Html = apply(Module, resource_not_found, []);
        I ->
            Id = proplists:get_value(id, I),
            M = tke_db:search(Project, message, Id),
            H = tke_db:search(Project, history, Id),
            Html = apply(Module, show_issue, [Project, I, M, H])
    end,
    %log:debug("Html=~p", [Html]),
    Html.

new_issue(Project, _Query) ->
    % TODO handle format (erlanf, html, etc.)
    tke_rendering_html:show_issue(Project, tke_db:get(Project, issue, empty), [], []).


% make a proplist from the query string
% x=111&y=bbb
% -> [{x, "111"}, {y, "bbb"}]
parse_query_string(undefined) -> [];
parse_query_string(Query_string) ->
    log:debug("Query_string=~p", [Query_string]),
    Q = string:tokens(Query_string, "&"),
    Q2 = [ string:tokens(X, "=") || X <- Q],
    % TODO protect from denial of service by restricting list_to_atom
    % to only allowed atoms
    [{X, Y} || [X, Y] <- Q2]. % make a proplist


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
            Id = N,
            I3 = [{id, Id} | I2]
    end,
    {ok, Id4} = tke_db:update(Project, issue, I3),
    log:debug("Id4=~p", [Id4]),
    case N of
        "new" ->
            Redirect_url = "/" ++ Project ++ "/issue/" ++ Id4,
            log:debug("redirect to: " ++ Redirect_url),
            {redirect, Redirect_url};
        N -> show_issue(Project, N, "")
    end;


% Login is a proplist 
http_post(["login"], Login) -> 
    log:debug("Login data: ~p", [Login]),
    Username = proplists:get_value("name",Login),
    Password = proplists:get_value("password",Login),
    Result = tke_user:check_user_login(Username, Password),
    log:debug("check_user_login: ~p", [Result]),
    case Result of
        {error, Reason} ->
            Cookie_item = [];
        ok ->
            % start session
            Cookie = yaws_api:new_cookie_session({user, Username}),
            log:debug("new_cookie_session: Cookie=~p", [Cookie]),
            Cookie_item = yaws_api:setcookie("sid", Cookie, "/")
    end,
    % TODO handle login
    [tke_html:login_page(), Cookie_item];

http_post(["new"], Post_data) -> 
    Project_name = proplists:get_value(name, Post_data),
    case tke_db:create_project(Project_name) of
        ok ->
            Redirect_url = "/" ++ Project_name ++ "/issue/",
            log:debug("redirect to: " ++ Redirect_url),
            {redirect, Redirect_url};
        _Else ->
            [{html, "500 - Internal Server Error"}, {status, 500}]
    end.


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
http_get([], _Query) -> tke_rendering_html:resource_not_found();
http_get(["login"], _Query) -> tke_rendering_html:login_page();
http_get([Project], _Query) -> no_resource(Project);
http_get([Project, "static" | Rest], Query) -> get_static_file(Project, Rest, Query);
http_get([Project, "issue"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", "list"], Query) -> list_issues(Project, Query);
http_get([Project, "issue", "new"], Query) -> new_issue(Project, Query);
http_get([Project, "issue", N], Query) -> show_issue(Project, N, Query);
http_get(A, B) -> log:info("Invalid GET request ~p ? ~p", [A, B]),
    tke_rendering_html:resource_not_found().

get_static_file(Project, Path, _Query) ->
    Filepath = Project ++ "/static/" ++ string:join(Path, "/"),
    %log:debug("Project=~p, Path=~p, Filepath=~p", [Project, Path, Filepath]),
    case file:read_file(Filepath) of
        {ok, Contents} -> {html, Contents};
        {error, Reason} ->
            log:debug("Could not get static file ~p: ~p", [Filepath, Reason]),
            {status, 404}
    end.


out(A) ->
    check_session(A),
    %log:debug("out(~p)", [A]),
    Method = A#arg.req#http_request.method,
    % TODO parse cookie / get session
    Url_tokens = string:tokens(A#arg.appmoddata, "/"),
    serve(Method, Url_tokens, A).

    %% old code for managing session

check_session(A) ->
    H = A#arg.headers,
    Cookie = H#headers.cookie,
    case yaws_api:find_cookie_val("sid", Cookie) of
        [] -> log:debug("no cookie found");
        C ->
            X = yaws_api:cookieval_to_opaque(C),
            log:debug("sid=~p", [C]),
            log:debug("Xcookie=~p", [X])
    end.


