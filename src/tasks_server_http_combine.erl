-module(tasks_server_http_combine).

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
    json_response/2,
    text_response/2
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
            {{<<"application">>, <<"json">>, []}, json_response},
            {{<<"text">>, <<"plain">>, []}, text_response}
        ],
        Req,
        State
    }.


json_request(Req0, State) ->
    {ok, ReqDataBin, Req} = cowboy_req:read_body(Req0),

    ReqData = tasks_server_util:from_json(ReqDataBin),
    {Code, Resp} = case tasks_server_schema:validate(tasks_combine, ReqData) of
        ok ->
            Tasks = maps:get(<<"tasks">>, ReqData),
            case tasks_server_util:tasks_combine(Tasks, <<"name">>, <<"requires">>, <<"command">>) of
                {ok, Cmd} ->
                    Resp1 = cowboy_req:set_resp_body(Cmd, Req),
                    % will reply with text/plain data: replace "json" header with "text"
                    Resp2 = cowboy_req:delete_resp_header(<<"content-type">>, Resp1),
                    Resp3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Resp2),
                    {200, Resp3};
                {error, {cycle, NamesCycle}} ->
                    Err = #{<<"error">> => <<"cycle">>, <<"tasks_cycle">> => NamesCycle},
                    {400, cowboy_req:set_resp_body(tasks_server_util:to_json(Err), Req)}
            end;
        error ->
            % invalid schema, report error
            Err = #{<<"error">> => <<"invalid_json_schema">>},
            {400, cowboy_req:set_resp_body(tasks_server_util:to_json(Err), Req)}
    end,

    cowboy_req:reply(Code, Resp),
    {stop, Resp, State}.


json_response(Req, State) ->
    {true, Req, State}.


text_response(Req, State) ->
    {true, Req, State}.

