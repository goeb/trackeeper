%% ----------------------------------
%% Copyright Frederic Hoerni 2013
%% ----------------------------------
%% Manage TKE database.
%%

%% @doc Module for TKE database
%%
%% @type issue() = proplist(). <p>An issue is referenced by an id.
%% It has at least the following fields: id, ctime, mtime, author.
%% <dl>
%% <dt><code>id</code></dt>
%% <dd>Identifier, used as unique key in the database</dd>
%% <dt><code>ctime</code></dt>
%% <dd>Creation time (UTC) of the issue, in the format datetime() as specified
%% in the module calendar.
%% Eg: {{2012,12,2},{13,54,8}}</dd>
%% <dt><code>mtime</code></dt>
%% <dd>Time of last modification (UTC). Same format as ctime.</dd>
%% <dt><code>author</code></dt>
%% <dd>Name of the user who created the issue. Format is string().</dd>
%% </dl>
%% </p>
%% @type message() = proplist(). <p>This type is used for messages.
%% A message is attached to an issue.
%% </p>
%% @type proplist() = [{atom(), any()}]. <p>Property lists are ordinary lists
%% containing entries in the form of either tuples, whose first elements are
%% keys used for lookup and insertion, or atoms, which work as shorthand for
%% tuples <code>{Atom, true}</code>.
%% </p>
%% <p>They can be managed via the functions of the module proplists.
%% </p>
%%
%% @end


-module(tke_db).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

-export([get/3, update/3, search/3]).
-export([create_project/1]).
-export([get_columns_automatic/0, get_column_properties/2, keep_columns/3]).

-include("tke_db.hrl").
-record(project, {name, issues, messages, history, structure}).
-record(history, {id, issue, author, ctime, action}).

% action = [new_issue,
%           {add_message, 34},
%           {delete_message, 434}, 
%           {add_file, 2},
%           {delete_file, 2},
%           {change_field, {Old, New}},

%% API -------------------

%% start as many processes as there are projects in the directory
%% @spec start(Directory::filename()) -> any()
start(Project) ->
    gen_server:start_link({local, registered_name(Project)},
                          tke_db, Project, []).

%% @spec stop(Directory::filename()) -> any()
stop(Project) -> gen_server:cast(registered_name(Project), stop).

%% @spec get(Project, Table, Id) -> Result
%%      Project     = string()
%%      Table       = issue | message
%%      Id          = integer()
%%      Result      = issue() | message()
%%
%% @doc Get an issue or message after its identifier.
get(Project, Table, N) when is_list(Project) ->
    gen_server:call(registered_name(Project), {get, Table, N}).

%% @spec update(Project, issue, Issue) -> Result
%%      Project     = string()
%%      Issue       = issue()
%%      Result      = any()
%%
%% @doc Update an existing issue or create a new one.
%% If the id of Issue is <code>undefined</code>, then a new issue is created.
%% Else the id is used to identify the issue to be updated.
update(Project, issue, Issue) ->
    log:debug("update0: "),
    gen_server:call(registered_name(Project), {update, Issue}).

%% @spec search(Project, Table, Search) -> Result
%%      Project     = string()
%%      Table       = issue | message
%%      Search      = proplist()
%%      Result      = {Columns, [issues()]}
%%      Columns     = [atom()]
%% @doc <p>Search for issues or messages according to a search description.
%% </p>
%% <p><code>Search</code> may contain the following search descriptors:</p>
%% <dl>
%% <dt><code>{columns, [atom()]} | {columns, all} | {columns, default}</code></dt>
%% <dd>Specify the columns needed in the result.</dd>
%% <dt><code>{search, Text}</code></dt>
%% <dd>Specify some text that should be present in all results.
%% This implies a full-text search on all text fields.</dd>
%% <dt><code>{keep, [{Column, Value}]} </code></dt>
%% <dd>Specify values of columns that must be kept in the result.</dd>
%% <dt><code>{exclude, [{Column, Value}]} </code></dt>
%% <dd>Specify values of columns that must be excluded from the result.</dd>
%% </dl>
%%
search(Project, Table, Search) ->
    gen_server:call(registered_name(Project), {search, Table, Search}).

create_project(Project) ->
    log:debug("create_project(~p)", [Project]),
    case file:make_dir(Project) of
        ok -> % populate with template project file
            case file:copy("priv/project_template/project", Project ++ "/" ++ project) of
                {ok, BytesCopied} ->
    %% TODO copy header, footer
                    % start a db server for this one
                    start(Project);
                {error, Reason} ->
                    log:error("Could not copy 'project' file: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            log:error("Could not create directory '~p': ~p", [Project, Reason]),
            {error, Reason}
    end.


%% Get properties of a column
%% (such as multi-select, etc.)
get_column_properties(Project, Column) ->
    gen_server:call(registered_name(Project), {get_column_properties, Column}).

%% Internals -------------------


registered_name(Project) ->
    list_to_atom("tke_" ++ Project).
    

%% Project = list(char)
init(Project) ->
    log:debug("Loading project ~p", [Project]),
    % TODO load from disk and populate an ets table
    Ctx = load(Project),
    log:debug("Loading project ~p completed", [Project]),
    {ok, Ctx}.

handle_call({get, issue, empty}, _From, Ctx) ->
    E = get_empty_issue(Ctx),
    log:debug("get_empty_issue: returned ~p", [E]),
    {reply, E, Ctx};
handle_call({get, issue, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.issues, N) of
        [I0] -> I = convert_to_proplist(Ctx, I0);
        [] -> I = undefined
    end,
    {reply, I, Ctx};
handle_call({get, message, N}, _From, Ctx) ->
    case ets:lookup(Ctx#project.messages, N) of
        [M0] -> %log:debug("found message ~p", [M0]),
            M = convert_to_proplist(Ctx, M0);
        [] -> M = undefined
    end,
    {reply, M, Ctx};
%% create new issue
%% Issue contains the message
handle_call({update, Issue}, _From, Ctx) ->
    log:debug("update: Issue=~p", [Issue]),
    Id0 = proplists:get_value(id, Issue),
    % TODO the diff of the issue wrt. to previous value
    Timestamp = get_timestamp(),
    case Id0 of 
        undefined ->
            {Longid, Shortid} = create_new_id(Ctx#project.issues, Issue),
            Id = Shortid,
            % ctime
            Ctime = Timestamp,
            % author
            Author = tke_user:get_author(),
            Issue2 = proplists:delete(id, Issue), % replace id
            Issue21 = [{id, Id}, {ctime, Ctime}, {author, Author},
                      {mtime, Timestamp} | Issue2];
        Id0 ->
            Id = Id0,
            [Old_issue_0] = ets:lookup(Ctx#project.issues, Id0),
            Oi = convert_to_proplist(Ctx, Old_issue_0),
            % report automatic fields of existing issue
            Ctime = proplists:get_value(ctime, Oi),
            Mtime = proplists:get_value(mtime, Oi),
            Author = proplists:get_value(author, Oi),
            Issue21 = [{ctime, Ctime}, {author, Author}, {mtime, Mtime}
                | Issue]
    end,
    Issue3 = [{mtime, Timestamp} | Issue21],
    % convert proplist to entry (~ record)
    I = convert_to_entry(Ctx, issue, Issue3),

    % make the diff (the summary of changes)
    case Id0 of
        undefined -> Diff = new;
        _Else ->
            [Old_issue] = ets:lookup(Ctx#project.issues, Id0),
            Diff = make_diff(tuple_to_list(Old_issue), tuple_to_list(I), []),
            log:debug("Diff: ~p", [Diff])
    end,

    case Diff of 
        [] -> % nothing new. do not update anything
            log:debug("nothing updated");
        Diff -> 
            ets:insert(Ctx#project.issues, I),
            log:debug("going to sync..."),
            sync(Ctx, I),
            % add history log
            add_history(Id, Diff, Timestamp, Ctx)
    end,
    % now add the message
    add_message(Id, Issue, Timestamp, Ctx),

    {reply, {ok, Id}, Ctx};

% Search : proplist
%   key 'columns' : list of columns needed in the return value
%   key 'search' : pattern for selective search
handle_call({search, issue, Search_description}, _From, Ctx) ->
    log:debug("search issue: Search=~p", [Search_description]),
  
    % filter
    case proplists:get_value(keep, Search_description) of
        undefined -> % take all records from the table
            Pattern_l = lists:duplicate(length(get_columns(Ctx, issue)), '_'),
            Pattern = list_to_tuple([issue | Pattern_l]);

        Keep -> % filter and keep only those specified
            Pattern = make_keeping_pattern(Keep, get_columns(Ctx, issue), [])
    end,
    log:debug("Filtering pattern: ~p", [Pattern]),
    Issues0 = ets:match_object(Ctx#project.issues, Pattern),

    % now search text if requested
    Search_text = proplists:get_value(search, Search_description),
    case Search_text of
        undefined -> Issues = Issues0;
        Search_text ->
            Issues0 = full_text_search(Issues0, Search_text, []),
            Messages = full_text_search(Ctx#project.messages, Search_text),
            Issues = merge_issues(Issues0, Ctx#project.issues, Messages)
    end,

    I_list = [convert_to_proplist(Ctx, I) || I <- Issues],
    % now keep only the needed columns
    Columns = proplists:get_value(columns, Search_description),
    case Columns of
        all -> Needed_columns = get_columns(Ctx, issue);
        default -> Needed_columns = get_default_columns(Ctx, issue);
        Needed_columns -> ok
    end,

    % do the sorting
    Sort = proplists:get_value(sort, Search_description),
    log:debug("search: Sort=~p", [Sort]),
    I_list2 = sort(I_list, Sort),
    {reply, {Needed_columns, I_list2}, Ctx};

%% Return messages that belong to the given issue id
handle_call({search, message, Issue_id}, _From, Ctx) ->
    Pattern = #message{issue=Issue_id, _ = '_'},
    log:debug("search ~p", [Pattern]),
    Messages = ets:match_object(Ctx#project.messages, Pattern),
    Mlist = [convert_to_proplist(Ctx, M) || M <- Messages],
    M_list2 = sort(Mlist, [id]),
    {reply, M_list2, Ctx};

%% Return history of a given issue
handle_call({search, history, Issue_id}, _From, Ctx) ->
    Pattern = #history{issue=Issue_id, _ = '_'},
    log:debug("search ~p", [Pattern]),
    History = ets:match_object(Ctx#project.history, Pattern),
    H_list = [convert_to_proplist(Ctx, H) || H <- History],
    H_list2 = sort(H_list, [id]),
    {reply, H_list2, Ctx};

handle_call({get_column_properties, Column}, _From, Ctx) ->
    Columns = proplists:get_value(issue_columns, Ctx#project.structure),
    Properties = proplists:get_value(Column, Columns),
    {reply, Properties, Ctx}.


%% Return proplists of issue, with all fields undefined
get_empty_issue(Ctx) ->
    Cols = get_columns(Ctx, issue),
    Values = lists:duplicate(length(Cols), undefined),
    I = list_to_tuple([issue | Values]), 
    convert_to_proplist(Ctx, I).

%% List : list of proplists
%% For each proplist, delete columns that are not mentioned in Columns
keep_columns([], _Columns, Acc) -> Acc;
keep_columns([Plist | Others], Columns, Acc) ->
    log:debug("keep_columns: Plist=~p, Columns=~p", [Plist, Columns]),
    {Lists, _Rest} = proplists:split(Plist, Columns),
    % remove the [x] as we only have unique keys here
    L2 = [X || [X] <- Lists],
    keep_columns(Others, Columns, [L2 | Acc]).


handle_cast(stop, Ctx) -> {stop, normal, Ctx};
handle_cast(_X, Y) -> {noreply, Y}.

%% File access functions
load(Project) ->
    Structure = load_project_file(Project),
    Issue_table = ets:new(issue,[private, {keypos, 2}]),
    Message_table = ets:new(message,[private, {keypos, 2}]),
    History_table = ets:new(history,[private, {keypos, 2}]),
    Ctx = #project{name=Project,
                   issues=Issue_table,
                   messages=Message_table,
                   history=History_table,
                   structure=Structure},
    load_issues(Ctx),
    Ctx.

load_project_file(Project) ->
    File = Project ++ "/project",
    {ok, Binary} = file:read_file(File),
    Term = decode_contents(Binary),
    log:debug("load_project_file(~p): done.", [Project]),
    Term.

load_issues(Ctx) ->
    {ok, Files} = file:list_dir(Ctx#project.name),
    Dirs = [Ctx#project.name ++ "/" ++ File || File <- Files],
    load_issues_from_dirs(Dirs, Ctx).

load_issues_from_dirs([], _Ctx) -> ok;
load_issues_from_dirs([Dir | Others], Ctx) ->
    File = Dir ++ "/issue",
    case file:read_file(File) of
        {error, _Reason} -> ok;
        {ok, Binary} ->
            Term = decode_contents(Binary),
            % convert proplist to record-like tuple
            I = convert_to_entry(Ctx, issue, Term),
            ets:insert(Ctx#project.issues, I)
            %log:debug("Issue ~p loaded.", [File])
    end,
    load_messages_and_history(Dir, Ctx#project.messages, Ctx#project.history),
    load_issues_from_dirs(Others, Ctx).

decode_contents(Binary) ->
    S = binary_to_list(Binary),
    {ok, Tokens, _EndLocation} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

load_messages_and_history(Dir, Messages, History) -> 
    case file:list_dir(Dir) of
        {ok, Files} -> load_from_files(Dir, Files, Messages, History);
        {error, _Reason} -> ok
    end.

%% Messages = ETS table containing messages
%% History = ETS table containing history events
load_from_files(_Dir, [], _Messages, _History) -> ok;
% Consider files starting with "msg."
load_from_files(Dir, [[$m, $s, $g, $. | Id] | Others], Messages, His) ->
    File = Dir ++ "/msg." ++ Id,
    %log:debug("Message ~p", [File]),
    case file:read_file(File) of
        {error, _Reason} -> log:debug("Message ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            case merge_to_record(message, Term) of % TODO is this really useful?
                {error, Reason} -> log:error("Cannot load message ~p: ~p",
                                             [Id, Reason]);
                {ok, T} -> ets:insert(Messages, T)
            end
    end,
    load_from_files(Dir, Others, Messages, His);

%% Load History
load_from_files(Dir, [[$h, $i, $s, $. | Id] | Others], Messages, History) ->
    File = Dir ++ "/his." ++ Id,
    case file:read_file(File) of
        {error, _Reason} -> log:debug("History ~p rejected", [File]);
        {ok, Binary} ->
            Term = decode_contents(Binary),
            ets:insert(History, Term)
    end,
    load_from_files(Dir, Others, Messages, History);

% other files, not starting with "msg."
load_from_files(Dir, [_F | Others], Messages, History) ->
    load_from_files(Dir, Others, Messages, History).


%% write to disk what has been modified
sync(Ctx, I) when element(1, I) == issue -> 
    log:debug("syncing..."),
    Id_str = get_value(Ctx, issue, id, I),
    Dirname = Ctx#project.name ++ "/" ++ Id_str,
    file:make_dir(Dirname),
    Filename = Dirname ++ "/issue",
    % convert record-like tuple to proplist
    Prop_i = convert_to_proplist(Ctx, I),
    sync_file(Filename, Prop_i);

sync(Ctx, M = #message{}) ->
    log:debug("syncing message: ~p", [M]),
    Issue = M#message.issue,
    Id = integer_to_list(M#message.id),
    Dirname = Ctx#project.name ++ "/" ++ Issue,
    Filename = Dirname ++ "/msg." ++ Id,
    sync_file(Filename, M);

sync(Ctx, H = #history{}) ->
    log:debug("syncing history: ~p", [H]),
    Issue = H#history.issue,
    Id = integer_to_list(H#history.id),
    Dirname = Ctx#project.name ++ "/" ++ Issue,
    Filename = Dirname ++ "/his." ++ Id,
    sync_file(Filename, H).


sync_file(Filename, Data) ->
    Str = io_lib:format("~p.", [Data]),
    Bytes = list_to_binary(Str),
    X=file:write_file(Filename, Bytes),
    log:debug("syncing...~p, Filename=~p", [X, Filename]),
    X.



code_change(_, _, _) -> ok.
handle_info(_, _) -> ok.
terminate(shutdown, _State) -> ok.

create_new_id(Table, Contents) ->
    Longid = tke_hash:create(Contents),
    Shortid = create_short_id(Table, Longid, 3),
    case Shortid of
        error -> % Longid probably already exixting
                 % recurse until a non used longid is found
                 create_new_id(Table, Contents);
        _Else -> ok
    end,
    log:debug("Longid=~s, Shortid=~s", [Longid, Shortid]),
    {Longid, Shortid}.

create_short_id(Table, Longid, N) when N > length(Longid) -> error;
create_short_id(Table, Longid, N) ->
    Shortid = string:substr(Longid, length(Longid)-(N-1), N),
    X = ets:lookup(Table, Shortid),
    log:debug("create_short_id: Shortid=~p, lookup=~p", [Shortid, X]),
    case X of
        [] -> % no previously existing such shortid
            Shortid;
        _Else -> % such shortid already existing
            create_short_id(Table, Longid, N+1)
    end.

create_short_id_test() ->
    T = ets:new(xxx, []),
    ets:insert(T, {"678"}),
    error = create_short_id(T, "12345678", 9),
    "78" = create_short_id(T, "12345678", 2),
    ets:insert(T, {"78"}),
    "5678" = create_short_id(T, "12345678", 2),
    "76" = create_short_id(T, "9876", 2),
    ets:delete(T).


%% Get new id functions for issue or message
get_new_id(Table) -> get_max_id(Table, ets:first(Table), 0) + 1.

get_max_id(_Table, '$end_of_table', N) -> N;
get_max_id(Table, Key, N) ->
    case Key > N of
        true -> Max = Key;
        _Else -> Max = N
    end,
    get_max_id(Table, ets:next(Table, Key), Max).

convert_to_proplist(Ctx, I) when element(1, I) == issue ->
    lists:zip(get_columns(Ctx, issue), tl(tuple_to_list(I)));
convert_to_proplist(_Ctx, M = #message{}) ->
    lists:zip(record_info(fields, message), tl(tuple_to_list(M)));
convert_to_proplist(_Ctx, H = #history{}) ->
    lists:zip(record_info(fields, history), tl(tuple_to_list(H))).

convert_to_record([], _Proplist, Record) ->
    list_to_tuple(lists:reverse(Record));
convert_to_record([Key | Others], Proplist, Record) ->
    Value = proplists:get_value(Key, Proplist),
    convert_to_record(Others, Proplist, [Value|Record]).

%% Project = list(char)  = Name of project
%% I       = proplists() = Issue to be converted
convert_to_entry(Ctx, issue, I) ->
    Keys = get_columns(Ctx, issue),
    convert_to_record(Keys, I, [issue]).

%% Sort a proplist according to a key
sort(I_list, undefined) -> I_list;
%% Col = atom() = Key used for the sorting of the proplist
sort(I_list, [Col]) ->
    % less than or equal function
    Lte = fun(A, B) ->
            proplists:get_value(Col, A) < proplists:get_value(Col, B) end,
    lists:sort(Lte, I_list);
   
%% TODO multi-column  sorting
sort(_I_list, _Sort) -> todo.

get_timestamp() ->
    TS = os:timestamp(),
    calendar:now_to_universal_time(TS).

%% Issue_id : id of related issue
%% Message = proplists() (contains also Issue info, but not needed here)
%% Ctx : context of the server    
add_message(Issue_id, Message, Timestamp, Ctx) ->
    Text = proplists:get_value(message, Message),
    Text_stripped = string:strip(Text),
    Id = get_new_id(Ctx#project.messages),
    M = #message{id=Id,
                 issue=Issue_id,
                 author="John Doe",
                 ctime=Timestamp,
                 text=Text_stripped},

    case Text_stripped of 
        "" -> % do not insert message in base (nothing has changed)
            log:debug("message not added");
        Text_stripped ->
            ets:insert(Ctx#project.messages, M),
            sync(Ctx, M)
    end,
    ok.

add_history(_Issue_id, [], _Timestamp, _Ctx) -> ok; % no diff, then do nothing
add_history(Issue_id, Diff, Timestamp, Ctx) ->
    Id = get_new_id(Ctx#project.history),
    H = #history{id=Id,
                 issue=Issue_id,
                 author="John Doe", % TODO
                 ctime=Timestamp,
                 action=Diff},
    ets:insert(Ctx#project.history, H),
    sync(Ctx, H).


%% Term = tuple()
merge_to_record(message, Term) ->
    case element(1, Term) of
        message -> 
            N = size(Term) - size(#message{}),
            case N of
                N when N > 0 -> T = delete_elements(Term, N);
                N when N == 0 -> T = Term;
                N when N < 0 -> T = add_elements(Term, -N)
            end,
            {ok, T};
        _Other -> {error, "Not a message structure"}
    end.

delete_elements(Tuple, 0) when is_tuple(Tuple) -> Tuple;
delete_elements(Tuple, N) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    Size = length(List),
    L2 = lists:sublist(List, 1, Size - N),
    list_to_tuple(L2).

add_elements(Tuple, 0) -> Tuple;
add_elements(Tuple, N) ->
    add_elements(erlang:append_element(Tuple, undefined), N-1).

%% Stucture of table issue
%% Columns are read from file <project-dir>/project and stored in Ctx
get_columns(Ctx, issue) ->
    Columns = proplists:get_value(issue_columns, Ctx#project.structure),
    % add "hidden" columns id, ctime, author  
    get_columns_automatic() ++ get_columns_mandatory() ++ 
        get_ordered_keys(Columns, []).

get_columns_automatic() -> [id, ctime, mtime, author].
get_columns_mandatory() -> [title].

get_default_columns(Ctx, issue) ->
    proplists:get_value(default_display, Ctx#project.structure).

%% Return the ordered list of keys of the given proplist.
%% This is not available from the proplists module
%% as proplists:get_keys/1 returns an unordered list.
get_ordered_keys([], Acc) -> lists:reverse(Acc);
get_ordered_keys([Atom | Rest], Acc) when is_atom(Atom) ->
    get_ordered_keys(Rest, [Atom | Acc]);    
get_ordered_keys([{Atom, _Value} | Rest], Acc) when is_atom(Atom) ->
    get_ordered_keys(Rest, [Atom | Acc]);
get_ordered_keys([_X | Rest], Acc) -> % ignore other cases
    get_ordered_keys(Rest, Acc).

get_value(Ctx, issue, id, I) ->
    Columns = get_columns(Ctx, issue),
    [issue | Values] = tuple_to_list(I),
    get_value(id, Columns, Values).

get_value(_Atom, _Columns = [], _Values = []) -> undefined;
get_value(Atom, [Atom | _Other_columns], [Value | _Other_values]) -> Value;
get_value(Atom, [_Col | Other_columns], [_Value | Other_values]) ->
    get_value(Atom, Other_columns, Other_values).


%% Make a diff of what changed between old and new issue
%% result is a proplists:
%% [{Key, {Old, New}}]
make_diff([], [], Acc) -> lists:reverse(Acc);
make_diff([Old_value | Rest_old], [New_value | Rest_new], Acc) ->
    case Old_value == New_value of
        true -> % no change
            make_diff(Rest_old, Rest_new, Acc);
        _Else -> % there is a change
            make_diff(Rest_old, Rest_new, [{Old_value,  New_value} | Acc])
    end.


%% Perform a full text search on the given list of tuples
full_text_search([], _Pattern, Found_issues) -> Found_issues;
full_text_search([Item | Rest], Pattern, Found_issues) ->
    case full_text_match(Item, Pattern, size(Item)) of
        found -> Found = [Item | Found_issues];
        not_found -> Found = Found_issues
    end,
    full_text_search(Rest, Pattern, Found).

%% Perform a full text search on the given ETS table
full_text_search(Table, Text) ->
    First = ets:first(Table),
    full_text_search(Table, First, Text, []).


full_text_search(Table, '$end_of_table', Pattern, Found_issues) -> Found_issues;
full_text_search(Table, Key, Pattern, Found_issues) ->
    [Item] = ets:lookup(Table, Key),
    % perform the search on the Key tuple
    log:debug("full_text_search: Key=~p", [Key]),
    case full_text_match(Item, Pattern, size(Item)) of
        found -> Found = [Item | Found_issues];
        not_found -> Found = Found_issues
    end,
    Next = ets:next(Table, Key),
    full_text_search(Table, Next, Pattern, Found).

%% Data = tuple() = the contents in which the search is performed
%% Pattern = the pattern that is searched for
full_text_match(Data, Pattern, 0) -> not_found;
full_text_match(Data, Pattern, N) ->
    Item = element(N, Data),
    if
        is_list(Item) -> Index = string:str(string:to_lower(Item), 
                                            string:to_lower(Pattern));
        true -> Index = 0
    end,
    case Index of
        0 -> % not found, iterate
            full_text_match(Data, Pattern, N-1);
        Index -> found
    end.
    

%% merge the list of issues with the issues
%% referenced by the messages
merge_issues(Issues, Table_issues, []) -> Issues;
merge_issues(Issues, Table_issues, [M | Rest]) ->
    Id = M#message.issue,
    case lookup_issue(Issues, Id) of
        found -> merge_issues(Issues, Table_issues, Rest);
        not_found -> % add the issue in the list
            [Issue] = ets:lookup(Table_issues, Id),
            merge_issues([Issue | Issues], Table_issues, Rest)
    end.
             
%% look if Id is present in issue list
lookup_issue([], Id) -> not_found;
lookup_issue([Issue | Rest], Id) ->
    case element(2, Issue) of
        Id -> found;
        _Else -> lookup_issue(Rest, Id)
    end.

%% convert format [{Key, Value}] to {issue, '_', '_', Value, etc.}
%%  Keep    = [{Key, Value}]
%%  Column  = [atom()]
%%  Key     = string()
%%  
make_keeping_pattern(_Keep, [], List) ->
    list_to_tuple([issue | lists:reverse(List)]);
make_keeping_pattern(Keep, [Column | Rest], List) ->
    Colname = atom_to_list(Column),
    case proplists:get_value(Colname, Keep) of
        undefined -> make_keeping_pattern(Keep, Rest, ['_' | List]);
        Value -> make_keeping_pattern(Keep, Rest, [Value | List])
    end.

