%%% ----------------------------------
%%% Copyright Frederic Hoerni 2012
%%% ----------------------------------
%%%
%%
%% @doc Module for Erlang rendering of TKE
%%
%% @type issue() = proplist(). <p>An issue is referenced by an id.</p>
%% @type message() = proplist(). <p>This type is used for messages.</p>
%% @type proplist() = [{atom(), any()}].<p>Property lists are ordinary lists
%% containing entries in the form of either tuples, whose first elements are
%% keys used for lookup and insertion, or atoms, which work as shorthand for
%% tuples <code>{Atom, true}</code>.
%% </p>
%% <p>They can be managed via the functions of the module proplists.
%%
%% @end

-module(tke_rendering_erlang).

-export([resource_not_found/0, list_issues/3]).
-export([show_issue/4]).
-export([login_page/0]).

resource_not_found() ->
    [{html, "404 - Resource not found"}, {status, 404}].

%% return text of a list of issues
%% @spec list_issues(Project, Columns, Issues) -> string()
%%      Project     = string()
%%      Columns     = [atom()]
%%      Issues      = [issues()]
%%
%% Issues contains all information of the issue. It is up to the rendering
%% module to keep only the columns specified in Columns.
list_issues(Project, Columns, Issues) ->
    IList = tke_db:keep_columns(Issues, Columns, []),
    Result = {Project, Columns, IList},
    {html, io_lib:format("~p", [Result])}.

% return text of an issue
show_issue(Project, Issue, Messages, History) ->
    Result = {Project, Issue, Messages, History},
    {html, io_lib:format("~p", [Result])}.

login_page() ->
    {html, "login page"}.

