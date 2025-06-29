-module(tasks_server_http_sort).

-behaviour(cowboy_rest).

% cowboy REST callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).

-export([
    json_request/2,
    json_response/2
]).

-include_lib("kernel/include/logger.hrl").


init(Req, State) ->
    {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, []}, json_request}
        ],
        Req,
        State
    }.


content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, []}, json_response}
        ],
        Req,
        State
    }.


json_request(Req0, State) ->
    {ok, ReqDataBin, Req} = cowboy_req:read_body(Req0),

    ReqData = tasks_server_util:from_json(ReqDataBin),
    {Code, Result} = case tasks_server_schema:validate(tasks_sort, ReqData) of
        ok ->
            Tasks = maps:get(<<"tasks">>, ReqData),
            case tasks_server_util:tasks_sort(Tasks, <<"name">>, <<"requires">>) of
                {ok, TasksSorted} ->
                    {200, #{<<"result">> => TasksSorted}};
                {error, {cycle, NamesCycle}} ->
                    {400, #{<<"error">> => <<"cycle">>, <<"tasks_cycle">> => NamesCycle}}
            end;
        error ->
            R = #{<<"error">> => <<"invalid_json_schema">>}, % TODO return more details
            {400, R}
    end,

    Resp = cowboy_req:set_resp_body(tasks_server_util:to_json(Result), Req),
    cowboy_req:reply(Code, Resp),
    {stop, Resp, State}.


json_response(Req, State) ->
    {true, Req, State}.

