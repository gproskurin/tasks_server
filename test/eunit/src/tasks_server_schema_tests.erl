-module(tasks_server_schema_tests).


-ifdef(TEST).

-export([setup/0, teardown/1]).

-include_lib("eunit/include/eunit.hrl").


setup() ->
    ok = tasks_server_schema:bootstrap().


teardown(_) ->
    ok.


schema_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            {"test schema", fun test_schema/0}
        ]
    }.


test_schema() ->
    ?assertEqual(
        ok,
        tasks_server_schema:validate(tasks_sort, #{<<"tasks">> => []})
    ),
    ?assertEqual(
        error,
        tasks_server_schema:validate(tasks_sort, #{<<"tasks">> => 42})
    ),
    ?assertEqual(
        ok,
        tasks_server_schema:validate(
            tasks_sort,
            #{
                <<"tasks">> => [
                    #{
                        <<"name">> => <<"t1">>,
                        <<"command">> => <<"c1">>,
                        <<"requires">> => [<<"t1">>, <<"t2">>]
                    }
                ]
            }
        )
    ).


-endif.

