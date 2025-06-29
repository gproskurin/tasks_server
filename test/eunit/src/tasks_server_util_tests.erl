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


tasks_sort_test() ->
    ?assertEqual({ok, []}, tasks_server_util:tasks_sort([], q, w)),

    Reconstruct = fun(Task) ->
        maps:without([<<"requires">>], Task)
    end,
    TasksSort = fun(Tasks) ->
        case tasks_server_util:tasks_sort(Tasks, <<"name">>, <<"requires">>) of
            {ok, R} -> lists:map(Reconstruct, R);
            {error, {cycle, _}} -> {error, cycle}
        end
    end,

    T1 = #{
        <<"name">> => <<"t1">>,
        <<"command">> => <<"touch /tmp/file1">>,
        <<"requires">> => [<<"t2">>]
    },
    T2 = #{
        <<"name">> => <<"t2">>,
        <<"command">> => <<"touch /tmp/file2">>,
        <<"requires">> => [<<"t3">>]
    },
    T3 = #{
        <<"name">> => <<"t3">>,
        <<"command">> => <<"touch /tmp/file3">>,
        <<"requires">> => [<<"t4">>]
    },
    T4 = #{
        <<"name">> => <<"t4">>,
        <<"command">> => <<"touch /tmp/file4">>
    },
    T5 = #{
        <<"name">> => <<"t5">>,
        <<"command">> => <<"touch /tmp/file5">>
    },

    % one independent task
    ?assertEqual(
        [Reconstruct(T4)],
        TasksSort([T4])
    ),

    % one independent taks and one dependent, in different order
    ?assertEqual(
        lists:map(Reconstruct, [T4,T3]),
        TasksSort([T4,T3])
    ),
    ?assertEqual(
        lists:map(Reconstruct, [T4,T3]),
        TasksSort([T3,T4])
    ),

    % cycle
    ?assertEqual(
        {error, cycle},
        TasksSort([T1,T2])
    ),
    ?assertEqual(
        {error, cycle},
        TasksSort([T2,T1])
    ),

    ?assertEqual(
        lists:map(Reconstruct, [T4, T3, T2, T1]),
        TasksSort([T3, T1, T2, T4])
    ),

    % two independent tasks
    ?assertEqual(
        lists:map(Reconstruct, [T4,T5]),
        TasksSort([T4,T5])
    ).


tasks_combine_test() ->
    TasksCombine = fun(Tasks) ->
        {ok, R} = tasks_server_util:tasks_combine(Tasks, <<"name">>, <<"requires">>, <<"command">>),
        erlang:iolist_to_binary(R)
    end,

    ?assertEqual(
        <<"#!/usr/bin/env bash\n">>,
        TasksCombine([])
    ),

    T1 = #{<<"name">> => <<"t1">>, <<"requires">> => [<<"t2">>], <<"command">> => <<"cmd1">>},
    T2 = #{<<"name">> => <<"t2">>, <<"command">> => <<"cmd2">>},
    Exp = <<"#!/usr/bin/env bash\ncmd2\ncmd1\n">>,
    ?assertEqual(
        Exp,
        TasksCombine([T1,T2])
    ),
    ?assertEqual(
        Exp,
        TasksCombine([T2,T1])
    ),

    % cycle
    Tcycle1 = #{<<"name">> => <<"t1">>,  <<"command">> => <<"c1">>},
    Tcycle2 = #{<<"name">> => <<"t2">>, <<"requires">> => [<<"t3">>], <<"command">> => <<"c2">>},
    Tcycle3 = #{<<"name">> => <<"t3">>, <<"requires">> => [<<"t2">>], <<"command">> => <<"c3">>},
    {error, {cycle, Tc}} = tasks_server_util:tasks_combine([Tcycle1,Tcycle2,Tcycle3], <<"name">>, <<"requires">>, <<"command">>),
    ?assertEqual(lists:sort([<<"t2">>,<<"t3">>]), lists:sort(Tc)).


-endif.

