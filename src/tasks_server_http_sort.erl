-module(tasks_server_http_sort).

-behaviour(cowboy_rest).

% cowboy REST callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2
]).

-export([
    handle_from_json/2
]).

-include_lib("kernel/include/logger.hrl").


init(Req, State) ->
    {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.


content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, []}, handle_from_json}
        ],
        Req,
        State
    }.


handle_from_json(Req, State) ->
    Resp = #{<<"hello">> => <<"from ts">>},
    {tasks_server_util:to_json(Resp), Req, State}.

