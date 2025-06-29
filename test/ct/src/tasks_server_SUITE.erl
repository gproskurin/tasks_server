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
        test_sort_real
    ]},
    {combine_tests, [parallel], [
        test_combine_empty
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
    Exp = #{<<"result">> => #{<<"tasks">> => []}},
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
    Data = #{<<"tasks">> => [
        #{
            <<"name">> => <<"t1">>,
            <<"command">> => <<"echo rm -rf /">>,
            <<"requires">> => []
        }
    ]},
    Exp = #{<<"result">> => Data},
    ?assertEqual(
        {200, Exp},
        http_post(<<"tasks/sort">>, Data)
    ).

test_combine_empty(_Config) ->
    ok.

test_stress(_Config) ->
    % TODO run many requests at the same time in parallel, for a few seconds
    ok.


-spec http_post(Path :: iodata(), Data :: map()) -> {Code :: integer(), Body :: map()}.
http_post(Path, Data) ->
    Url = [<<"http://localhost:">>, erlang:integer_to_binary(?TASKS_SERVER_PORT), $/, Path],
    ct:pal("URL:~p", [Url]),
    ct:pal("URL_FULL:~p", [erlang:iolist_to_binary(Url)]),
    Headers = [{"Accept", "application/json"}],
    {ok, {{_, Code, _}, _Headers, Body}} = httpc:request(
        post,
        {Url, Headers, "application/json", tasks_server_util:to_json(Data)},
        [],
        []
    ),
    {Code, tasks_server_util:from_json(Body)}.

