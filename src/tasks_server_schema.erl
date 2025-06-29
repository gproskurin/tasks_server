-module(tasks_server_schema).

-export([
    bootstrap/0,
    validate/2
]).

-define(SCHEMA_REFS, [tasks_sort, tasks_combine]).


schema_name(SchemaRef) ->
    erlang:atom_to_list(SchemaRef).


bootstrap() ->
    lists:foreach(
        fun(Ref) ->
            ok = jesse:add_schema(schema_name(Ref), schema(Ref))
        end,
        ?SCHEMA_REFS
    ).


schema(tasks_sort) ->
    #{
        <<"type">> => <<"object">>,
        <<"additionalProperties">> => true,
        <<"properties">> => #{
            <<"tasks">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => schema_task()
            }
        },
        <<"required">> => [<<"tasks">>]
    };

schema(tasks_combine) ->
    #{
        <<"type">> => <<"object">>,
        <<"additionalProperties">> => true,
        <<"properties">> => #{
            <<"tasks">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => schema_task()
            }
        },
        <<"required">> => [<<"tasks">>]
    }.

schema_task() ->
    #{
        <<"type">> => <<"object">>,
        <<"additionalProperties">> => false,
        <<"required">> => [<<"name">>, <<"command">>],
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"command">> => #{<<"type">> => <<"string">>},
            <<"requires">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{
                    <<"type">> => <<"string">>
                }
            }
        }
    }.


validate(SchemaRef, Data) ->
    case jesse:validate(schema_name(SchemaRef), Data) of
        {ok, _} -> ok;
        {error, _} -> error
    end.

