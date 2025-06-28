-module(tasks_server_util_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


json_test() ->
    ?assertEqual(<<"{}">>, tasks_server_util:to_json(#{})),
    ?assertEqual(#{}, tasks_server_util:from_json(<<"{}">>)),

    ?assertEqual(
        #{<<"a">> => 1, <<"b">> => [2, 3]},
        tasks_server_util:from_json(<<"{\"a\":1, \"b\":[2,3]}">>)
    ),

    M = #{
        <<"a">> => 1,
        <<"b">> => #{
            <<"c">> => [1, <<"x">>]
        }
    },
    ?assertEqual(
        M,
        tasks_server_util:from_json(tasks_server_util:to_json(M))
    ).


-endif.

