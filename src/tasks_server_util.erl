-module(tasks_server_util).

-export([
    from_json/1,
    to_json/1,

    tasks_sort/3
]).


from_json(D) ->
    jiffy:decode(D, [return_maps]).


to_json(D) ->
    jiffy:encode(D).


% index list of maps by some key
maps_list_idx(Maps, Key) ->
    lists:foldl(
        fun(#{Key := V} = M, Acc) ->
            Acc#{V => M}
        end,
        #{},
        Maps
    ).


tasks_deps(Tasks, NameKey, DepsKey) ->
    lists:foldl(
        fun(Task, Acc) ->
            Name = maps:get(NameKey, Task),
            Deps = maps:get(DepsKey, Task, []),
            Acc#{Name => sets:from_list(Deps)}
        end,
        #{},
        Tasks
    ).


tasks_add_iter(NamesTodo, AllNamesAdded, TasksDeps, Result) ->
    R = lists:foldl(
        fun(Name, {AccHaveProgress, AccResult, AccSkipped, AccAllNamesAdded}) ->
            Deps = maps:get(Name, TasksDeps),
            case sets:is_subset(Deps, AccAllNamesAdded) of
                true ->
                    % all deps of "Name" task are already added, we can add it to "added" result
                    % made some progress
                    {true, [Name|AccResult], AccSkipped, sets:add_element(Name, AccAllNamesAdded)};
                false ->
                    % not all deps are added, skip for now
                    {AccHaveProgress, AccResult, [Name|AccSkipped], AccAllNamesAdded}
            end
        end,
        {false, Result, [], AllNamesAdded},
        NamesTodo
    ),
    case R of
        {false, _, _, _} ->
            % no progress made
            false;
        {true, A1, A2, A3} ->
            % made some progress, return updated accumulators
            {A1, A2, A3}
    end.


tasks_sort_impl([_|_] = NamesTodo, AllNamesAdded, TasksDeps, Result) ->
    case tasks_add_iter(NamesTodo, AllNamesAdded, TasksDeps, Result) of
        false ->
            % nothing was added, we have a cycle
            {error, {cycle, NamesTodo}};

        {ResultNew, [] = _NamesSkipped, _} ->
            % nothing skipped, finish
            {ok, ResultNew};

        {ResultNew, [_|_] = NamesSkipped, AllNamesAddedNew} ->
            % something was added, something was skipped, reiterate on "skipped"
            tasks_sort_impl(NamesSkipped, AllNamesAddedNew, TasksDeps, ResultNew)
    end.


tasks_sort([] = _Tasks, _NameKey, _DepsKey) ->
    {ok, []};
tasks_sort(Tasks, NameKey, DepsKey) ->
    NamesTodo = [N || #{NameKey := N} <- Tasks],
    AllNamesAdded = sets:new(),
    TasksDeps = tasks_deps(Tasks, NameKey, DepsKey),

    case tasks_sort_impl(NamesTodo, AllNamesAdded, TasksDeps, []) of
        {ok, ResultReversed} ->
            TasksIdx = maps_list_idx(Tasks, NameKey),
            % tasks_sort_impl prepends items to result, hence returns result reversed
            % doing one pass via result with foldl reverses it again
            Result = lists:foldl(
                fun(Task, Acc) ->
                    [task_reconstruct(Task, TasksIdx) | Acc]
                end,
                [],
                ResultReversed
            ),
            {ok, Result};
        {error, {cycle, _Skipped}} = Err ->
            Err
    end.


% given task name, return task's data, do some cleanup if necessary
task_reconstruct(Name, TasksIdx) ->
    Task = maps:get(Name, TasksIdx),
    maps:without([<<"requires">>], Task). % cleanup result


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

maps_list_idx_test() ->
    ?assertEqual(#{}, maps_list_idx([], k)),

    M1 = #{key => 1, some_data => 11},
    M2 = #{key => <<"qw">>, other_data => 22},
    ?assertEqual(
       #{1 => M1, <<"qw">> => M2},
       maps_list_idx([M1,M2], key)
    ).


tasks_deps_test() ->
    ?assertEqual(#{}, tasks_deps([], q, w)),

    SetEqList = fun(S, L) ->
        lists:sort(L) == lists:sort(sets:to_list(S))
    end,

    T1 = #{n => n1, <<"req">> => [q,w,e]},
    T2 = #{n => n2, <<"req">> => []},
    T3 = #{n => n3},
    #{n1 := Deps1, n2 := Deps2, n3 := Deps3} = tasks_deps([T1,T2,T3], n, <<"req">>),
    ?assert(SetEqList(Deps1, [q,w,e])),
    ?assert(SetEqList(Deps2, [])),
    ?assert(SetEqList(Deps3, [])).


-endif.

