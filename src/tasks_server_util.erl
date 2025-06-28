-module(tasks_server_util).

-export([
    from_json/1,
    to_json/1
]).


from_json(D) ->
    jiffy:decode(D, [return_maps]).


to_json(D) ->
    jiffy:encode(D).

