tasks_server
=====

Tasks Server

Requirements
-----

Tested with Erlang 26.2.5.13 and 28.0.1

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 eunit
    $ rebar3 ct

Start and test manually 
-----

    # start application in separate terminal
    $ rebar3 shell

    $ curl -v -H 'content-type: application/json' http://localhost:32800/tasks/sort -X POST -d @example_tasks_post.json
    $ curl -v -H 'content-type: application/json' http://localhost:32800/tasks/combine -X POST -d @example_tasks_post.json

