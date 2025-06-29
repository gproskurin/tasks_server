-module(tasks_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("include/tasks_server.hrl").

-compile([export_all, nowarn_export_all]).


all() -> [
    {group, sort_tests},
    {group, combine_tests},
    {group, stress_tests}
].

groups() -> [
    {sort_tests, [parallel], [
        test_sort_empty,
        test_sort_invalid_schema,
        test_sort_real,
        test_sort_cycle
    ]},
    {combine_tests, [parallel], [
        test_combine_empty,
        test_combine_invalid_schema,
        test_combine_real,
        test_combine_cycle
    ]},
    {stress_tests, [], [
        test_stress
    ]}
].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(tasks_server),
    {ok, _} = application:ensure_all_started(inets),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(tasks_server).


test_sort_empty(_Config) ->
    Data = #{<<"tasks">> => []},
    Exp = #{<<"result">> => []},
    ?assertEqual(
        {200, Exp},
        http_post(<<"tasks/sort">>, Data)
    ).


test_sort_invalid_schema(_Config) ->
    Data = #{<<"tasks">> => 42},
    ?assertMatch(
        {400, #{<<"error">> := <<"invalid_json_schema">>}},
        http_post(<<"tasks/sort">>, Data)
    ).


test_sort_real(_Config) ->
    ExpTask = fun(T) -> maps:without([<<"requires">>], T) end,

    T1 = #{
        <<"name">> => <<"t1">>,
        <<"command">> => <<"echo rm -rf /">>,
        <<"requires">> => [<<"t2">>]
    },
    T2 = #{
        <<"name">> => <<"t2">>,
        <<"command">> => <<"echo rm -rf /task_data_2">>
    },

    Exp = #{<<"result">> => [ExpTask(T2), ExpTask(T1)]},

    Data1 = #{<<"tasks">> => [T1,T2]},
    ?assertEqual(
        {200, Exp},
        http_post(<<"tasks/sort">>, Data1)
    ),

    Data2 = #{<<"tasks">> => [T2,T1]},
    ?assertEqual(
        {200, Exp},
        http_post(<<"tasks/sort">>, Data2)
    ).

test_sort_cycle(_Config) ->
    T0 = #{
        <<"name">> => <<"t0">>,
        <<"command">> => <<"c0">>
    },
    T1 = #{
        <<"name">> => <<"t1">>,
        <<"command">> => <<"c1">>,
        <<"requires">> => [<<"t2">>]
    },
    T2 = #{
        <<"name">> => <<"t2">>,
        <<"command">> => <<"c2">>,
        <<"requires">> => [<<"t1">>]
    },

    Data = #{<<"tasks">> => [T0, T1, T2]},
    {400, #{<<"error">> := <<"cycle">>, <<"tasks_cycle">> := TasksCycle}} =
        http_post(<<"tasks/sort">>, Data),
    ?assertEqual(lists:sort([<<"t1">>,<<"t2">>]), lists:sort(TasksCycle)).


test_combine_empty(_Config) ->
    ?assertEqual(
        {200, <<"#!/usr/bin/env bash\n">>},
        http_post(<<"tasks/combine">>, #{<<"tasks">> => []})
    ).

test_combine_invalid_schema(_Config) ->
    Task = #{<<"name">> => <<"n">>}, % no "command"
    Data = #{<<"tasks">> => [Task]},
    ?assertMatch(
        {400, #{<<"error">> := <<"invalid_json_schema">>}},
        http_post(<<"tasks/combine">>, Data)
    ).

test_combine_real(_Config) ->
    T1 = #{<<"name">> => <<"t1">>, <<"requires">> => [<<"t2">>], <<"command">> => <<"c1">>},
    T2 = #{<<"name">> => <<"t2">>, <<"command">> => <<"c2">>},
    ?assertEqual(
        % single task
        {200, <<"#!/usr/bin/env bash\nc2\n">>},
        http_post(<<"tasks/combine">>, #{<<"tasks">> => [T2]})
    ),
    ?assertEqual(
        % two tasks
        {200, <<"#!/usr/bin/env bash\nc2\nc1\n">>},
        http_post(<<"tasks/combine">>, #{<<"tasks">> => [T1,T2]})
    ).

test_combine_cycle(_Config) ->
    T1 = #{<<"name">> => <<"t1">>, <<"requires">> => [<<"t2">>], <<"command">> => <<"c1">>},
    T2 = #{<<"name">> => <<"t2">>, <<"requires">> => [<<"t1">>], <<"command">> => <<"c2">>},
    Data = #{<<"tasks">> => [T1,T2]},
    {400, #{<<"error">> := <<"cycle">>, <<"tasks_cycle">> := Tc}} = http_post(<<"tasks/combine">>, Data),
    ?assertEqual(lists:sort([<<"t1">>,<<"t2">>]), lists:sort(Tc)).


test_stress(_Config) ->
    % TODO run many requests at the same time in parallel, for a few seconds
    ok.


-spec http_post(Path :: iodata(), Data :: map()) -> {Code :: integer(), Body :: map() | binary()}.
http_post(Path, Data) ->
    Url = [<<"http://localhost:">>, erlang:integer_to_binary(?TASKS_SERVER_PORT), $/, Path],
    ct:pal("URL:~p", [Url]),
    ct:pal("URL_FULL:~p", [erlang:iolist_to_binary(Url)]),
    Headers = [{"Accept", "application/json, test/plain"}],
    R =  httpc:request(
        post,
        {Url, Headers, "application/json", tasks_server_util:to_json(Data)},
        [],
        []
    ),
    ct:pal("RESULT: ~p", [R]),
    {ok, {{_, Code, _}, RespHeaders, Body}} = R,
    RespData = case proplists:get_value("content-type", RespHeaders) of
        "application/json" -> tasks_server_util:from_json(Body);
        "text/plain" -> erlang:iolist_to_binary(Body)
    end,
    {Code, RespData}.

