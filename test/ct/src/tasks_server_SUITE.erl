-module(tasks_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).


all() -> [
    {group, sort_tests},
    {group, combine_tests}
].

groups() -> [
    {sort_tests, [parallel], [
        test_sort_empty
    ]},
    {combine_tests, [parallel], [
        test_combine_empty
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


test_combine_empty(_Config) ->
    ok.


-spec http_post(Path :: iodata(), Data :: map()) -> {Code :: integer(), Body :: map()}.
http_post(Path, Data) ->
    Url = [<<"http://localhost:30800/">>, Path],
    Headers = [{"Accept", "application/json"}],
    {ok, {{_, Code, _}, _Headers, Body}} = httpc:request(
        post,
        {Url, Headers, "application/json", tasks_server_util:to_json(Data)},
        [],
        []
    ),
    {Code, tasks_server_util:from_json(Body)}.

