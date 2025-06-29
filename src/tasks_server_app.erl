%%%-------------------------------------------------------------------
%% @doc tasks_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tasks_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/tasks_server.hrl").


start(_StartType, _StartArgs) ->
    ok = tasks_server_schema:bootstrap(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/tasks/sort">>, tasks_server_http_sort, []},
            {<<"/tasks/combine">>, tasks_server_http_combine, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        tasks_server_http_listener,
        [{port, ?TASKS_SERVER_PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    tasks_server_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
