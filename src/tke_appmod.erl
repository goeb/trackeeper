-module(tke_appmod).

-include("../../../yaws/include/yaws_api.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("tke_db.hrl").
-include("tke_appmod.hrl").
-compile(export_all).



%% init server (called by yaws if runmod is specified)
start() ->
    io:format("~p starting...\n", [ ?MODULE ]),
    tke_db:start("tke"), % TODO start as many tke_db as there are projects
    tke_db:start("maison"), % TODO start as many tke_db as there are projects
    tke_user:start(),
    ok.

% action when no ressource is given
no_resource(_Project_name) -> tke_rendering_html:resource_not_found().

list_issues(Webctx, Query_params) ->
    Project_name = Webctx#webctx.project,
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
    apply(Module, list_issues, [Webctx, Columns, Issues]).

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
    L = parse_criteria(Sorting),
    [{X, list_to_existing_atom(Y)} || {X, Y} <- L].

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
show_issue(Webctx, Issue_id, Query) ->
    Project = Webctx#webctx.project,
    log:debug("show_issue(~p, ~p, ~p)", [Project, Issue_id, Query]),
    Module = get_format(Query),
    I = tke_db:get(Project, issue, Issue_id),
    case I of
        undefined -> Html = apply(Module, resource_not_found, []);
        I ->
            Id = proplists:get_value(id, I),
            M = tke_db:search(Project, message, Id),
            H = tke_db:search(Project, history, Id),
            Html = apply(Module, show_issue, [Webctx, I, M, H])
    end,
    %log:debug("Html=~p", [Html]),
    Html.

new_issue(Webctx, _Query) ->
    % TODO handle format (erlanf, html, etc.)
    Project = Webctx#webctx.project,
    tke_rendering_html:show_issue(Webctx, tke_db:get(Project, issue, empty), [], []).


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


serve('GET', Url_tokens, Session_id, Http_req) -> 
    Q = parse_query_string(Http_req#arg.querydata),
    http_get(Url_tokens, Session_id, Q);

serve('POST', Url_tokens, Session_id, Http_req) ->
    log:debug("Http_req=~p", [Http_req]),
    log:debug("content_type=~p", [Http_req#arg.headers#headers.content_type]),

    case string:str(Http_req#arg.headers#headers.content_type, "multipart/form-data") of
        0 -> % not found. This is not a "multipart/form-data"
            Data = yaws_api:parse_post(Http_req),
            http_post(Url_tokens, Data, Session_id);
        _Else ->
            case yaws_api:parse_multipart_post(Http_req) of
                {result, Data} -> 
                        % consolidate Multipart_data
                        Data_proplist = consolidate_multipart(Data, []),
                        log:debug("Data_proplist=~p", [Data_proplist]),
                        http_post(Url_tokens, Data_proplist, Session_id);
                {cont, Cont, _Res} ->
                    % TODO accumulate data
                    {get_more, Cont, todo}
            end
    end.
    %Post_data = yaws_api:parse_post(Http_req),
    %log:debug("Post: Multipart=~p", [Multipart]),
    %log:debug("Post: Post_data=~p", [Post_data]),

%% Process POST requests
%% Request for creating new project
% Login is a proplist 
http_post(["login"], Login, _Old_sid) -> 
    log:debug("Login data: ~p", [Login]),
    Username = proplists:get_value("name", Login),
    Password = proplists:get_value("password", Login),
    Result = tke_user:user_login(Username, Password),
    log:debug("user_login: ~p", [Result]),
    case Result of
        {error, Reason} ->
            Cookie_item = [];
        {ok, Sid} -> % start session (Sid = session id)
            Cookie_item = yaws_api:set_cookie("sid", Sid, [{max_age, 3600}])
    end,
    % TODO handle login
    [tke_rendering_html:login_page(), Cookie_item];

http_post(_Resource, _Data, no_session_id) -> {redirect, "/login"};
http_post(["new"], Form_data, Sid) -> 
    log:debug("http_post: new, data=~p", [Form_data]),
    case proplists:get_value("project_name", Form_data) of
        undefined -> 
            {redirect, "/"};
        Project_name ->
            ok = tke_db:create_project(Project_name),
            {redirect, "/" ++ Project_name}
    end;
http_post([Project, "issue", N], Issue, Sid) -> 
    Webctx = #webctx{project=Project, sid=Sid},
    %% 'id' key normally not needed as it is given by N
    I2 = proplists:delete(id, Issue),
    Username = tke_user:get_user(Sid),
    case N of
        "new" -> % set id = undefined
            I3 = [{id, undefined}, {username, Username} | I2];
        N -> 
            Id = N,
            I3 = [{id, Id} | I2]
    end,

    I4 = [{username, Username} | I3],
    {ok, Id4} = tke_db:update(Project, issue, I4),
    log:debug("Id4=~p", [Id4]),
    case N of
        "new" ->
            Redirect_url = "/" ++ Project ++ "/issue/" ++ Id4,
            log:debug("redirect to: " ++ Redirect_url),
            {redirect, Redirect_url};
        N -> show_issue(Webctx, N, "")
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
http_get(["login"], _Sid, _Query) -> tke_rendering_html:login_page();
http_get(_Resource, no_session_id, _Query) -> {redirect, "/login"};
http_get([], Sid, _Query) -> tke_rendering_html:project_config();
http_get([Project], Sid, _Query) -> no_resource(Project);
http_get([Project, "static" | Rest], Sid, Query) -> get_static_file(Project, Rest, Query);
http_get([Project, "issue"], Sid, Query) ->
    Webctx = #webctx{project=Project, sid=Sid},
    list_issues(Webctx, Query);
http_get([P, "issue", "list"], Sid, Q) -> http_get([P, "issue"], Sid, Q);
http_get([Project, "issue", "new"], Sid, Query) ->
    Webctx = #webctx{project=Project, sid=Sid},
    new_issue(Webctx, Query);
http_get([Project, "issue", N], Sid, Query) ->
    Webctx = #webctx{project=Project, sid=Sid},
    show_issue(Webctx, N, Query);
http_get(A, Sid, B) -> log:info("Invalid GET request ~p ? ~p", [A, B]),
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
    Session_id = get_session_id(A),
    case tke_user:get_user(Session_id) of
        undefined -> Sid = no_session_id;
        _Else -> % valid logged in user
            Sid = Session_id
    end,
    Method = A#arg.req#http_request.method,
    Url_tokens = string:tokens(A#arg.appmoddata, "/"),
    serve(Method, Url_tokens, Sid, A).

get_session_id(A) ->
    H = A#arg.headers,
    Cookie = H#headers.cookie,
    case yaws_api:find_cookie_val("sid", Cookie) of
        [] -> Session_id = undefined;
        Session_id -> ok
    end,
    log:debug("get_session_id() -> ~p", [Session_id]),
    Session_id.

%% "aa+bb-cc" -> [{'+', aa}, {'+', bb}, {'-', cc}]
parse_criteria(undefined) -> [];
parse_criteria([$+ | String]) -> parse_criteria(String, '+', "", []);
parse_criteria([$- | String]) -> parse_criteria(String, '-', "", []);
parse_criteria(String) -> parse_criteria(String, '+', "", []).

%% parse_criteria (accumulator version)
parse_criteria([], _Sign, "", Acc) -> lists:reverse(Acc);
parse_criteria([], Sign, Token, Acc) ->
    Current = {Sign, Token},
    lists:reverse([Current | Acc]);
parse_criteria([$- | Rest], _S, "", Acc) -> parse_criteria(Rest, '-', "", Acc);
parse_criteria([$+ | Rest], _S, "", Acc) -> parse_criteria(Rest, '+', "", Acc);
parse_criteria([$- | Rest], Sign, Token, Acc) ->
    parse_criteria(Rest, '-', "", [{Sign, Token} | Acc]);
parse_criteria([$+ | Rest], Sign, Token, Acc) ->
    parse_criteria(Rest, '+', "", [{Sign, Token} | Acc]);
parse_criteria([Char | Rest], Sign, Token, Acc) ->
    parse_criteria(Rest, Sign, Token ++ [Char], Acc).

parse_criteria_test() ->
    [{'+', "aa"}, {'+', "bb"}, {'-', "cc"}] = parse_criteria("aa+bb-cc"),
    [{'-', "dd"}] = parse_criteria("-dd"),
    [{'-', "dd"}] = parse_criteria("-dd+"),
    [{'-', "dd"}, {'+', "aaa%"}] = parse_criteria("--dd++aaa%-"),
    [{'+', "xx"}, {'-', "yy"}, {'+', "zz"}] = parse_criteria("+xx-yy+zz"),
    [] = parse_criteria(""),
    [] = parse_criteria(undefined),
    [{'+', "uuuu"}] = parse_criteria("uuuu").



