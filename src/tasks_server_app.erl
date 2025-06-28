%%%-------------------------------------------------------------------
%% @doc tasks_server public API
%% @end
%%%-------------------------------------------------------------------

-module(tasks_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/tasks/sort">>, tasks_server_http_sort, []}
            %{<<"/tasks/combine">>, tasks_server_http_combine, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        tasks_server_http_listener,
        [{port, 30800}],
        #{env => #{dispatch => Dispatch}}
    ),
    tasks_server_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
