-module(route_spec_server_tests).
-include_lib("eunit/include/eunit.hrl").

route_spec_fixture() ->
    #{
      <<"routes">> => [
                       #{
                         <<"regex">> => <<"/path/foo">>,
                         <<"id">> => 10,
                         <<"enabled">> => true,
                         <<"host">> => <<"http://10:8010">>
                        },
                       #{
                         <<"regex">> => <<"/path/bar">>,
                         <<"id">> => 20,
                         <<"enabled">> => false,
                         <<"host">> => <<"http://20:8020">>
                        },
                       #{
                         <<"regex">> => <<"/path/bar/foo">>,
                         <<"id">> => 30,
                         <<"enabled">> => true,
                         <<"host">> => <<"http://30:8030">>
                        },
                       #{
                         <<"regex">> => <<"/path/noport">>,
                         <<"id">> => 40,
                         <<"enabled">> => true,
                         <<"host">> => <<"http://40">>
                        }
                      ],
      <<"default">> => <<"http://localhost:8000">>
     }.

start() ->
    {ok, Pid} = route_spec_server:start_link(route_spec_fixture()),
    ?debugFmt("start: ~p", [Pid]),
    Pid.

stop(Pid) ->
    ?debugFmt("stop: ~p", [Pid]),
    %% gen_server:call(terminate, Pid).
    route_spec_server:stop().

test_default_routes(_) ->
    [
     ?_assertEqual({"10", 8010}, route_spec_server:match_server(<<"/path/foo">>)),
     ?_assertEqual({"localhost", 8000}, route_spec_server:match_server(<<"/path/bar">>)),
     ?_assertEqual({"30", 8030}, route_spec_server:match_server(<<"/path/bar/foo">>)),
     ?_assertEqual({"40", 80}, route_spec_server:match_server(<<"/path/noport">>)),
     ?_assertEqual({"localhost", 8000}, route_spec_server:match_server(<<"/bs">>))
    ].

test_disable_route(Pid) ->
    ?debugFmt("test_disable:~p", [Pid]),
    ok = route_spec_server:disable_routespec(10),
    Host = route_spec_server:match_server(<<"/path/foo">>),
    [?_assertEqual({"localhost", 8000}, Host)].

test_enable_route(Pid) ->
    ?debugFmt("test_enable:~p", [Pid]),
    ok = route_spec_server:enable_routespec(20),
    Host = route_spec_server:match_server(<<"/path/bar">>),
    [?_assertEqual({"20", 8020}, Host)].

route_spec_test_()->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun test_default_routes/1,
      fun test_disable_route/1,
      fun test_enable_route/1
     ]
    }.
