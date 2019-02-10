-module(poller_server_tests).
-include_lib("eunit/include/eunit.hrl").


handle_fixture_portpath(Port) when is_integer(Port) ->
    handle_fixture_portpath({Port, <<"/">>});
handle_fixture_portpath({Port, Path}) ->
    {integer_to_binary(Port), Path}.

alias_fixture(Blue, Green) ->
    {BPort, BPath} = handle_fixture_portpath(Blue),
    {GPort, GPath} = handle_fixture_portpath(Green),
    #{<<"spandex">> =>
          #{<<"hosts">> =>
                #{
                  <<"blue">> => <<"http://localhost:", BPort/binary, BPath/binary>>,
                  <<"green">> => <<"http://localhost:", GPort/binary, GPath/binary>>
                 }
           }
     }.

start_poller() ->
    {ok, _} = application:ensure_all_started(gun).

stop_poller(PidPortList) ->
    ok = test_utils:stop_http(PidPortList).

test_poll_hosts(PidPortList, Expected) ->
    test_poll_hosts(PidPortList, [<<"/">>, <<"/">>], Expected).


test_poll_hosts([{_, BluePort}, {_, GreenPort}], Paths, Expected) ->
    %% this test doesn't start the poller server process
    [Blue, Green] = lists:zip([BluePort, GreenPort], Paths),
    {ok, {State, timeout}} = poller_server:init({alias_fixture(Blue, Green), timeout}),
    MakeURL = fun(Port, Path) ->
                      P = erlang:integer_to_binary(Port),
                      <<"http://localhost:", P/binary, Path/binary>>
              end,
    ExpectedState = maps:from_list(
                      [
                       {MakeURL(Port, Path), S} ||
                          {{Port, Path}, S} <- lists:zip([Blue, Green], Expected)
                      ]),
    StateMap = maps:from_list(
                 [
                  {URL, HostState} || {URL, HostState, _} <- poller_server:poll_hosts(State, 20)
                 ]),
    ?_assertEqual(ExpectedState, StateMap).


polling_happy_path_test_()->
    {"tests 2 backends are correctly detected as up",
     spawn,
     {setup,
      fun () -> start_poller(), test_utils:start_http(2) end,
      fun stop_poller/1,
      fun (PidPortList) -> test_poll_hosts(PidPortList, [up, up]) end
     }
    }.


polling_blue_down_test_()->
    {"tests 1 of 2 backends is correctly detected as down",
     spawn,
     {setup,
      fun () -> start_poller(), [{null, 1} | test_utils:start_http(1)] end,
      fun stop_poller/1,
      fun (PidPortList) -> test_poll_hosts(PidPortList, [down, up]) end
     }
    }.


polling_blue_404_test_()->
    {"test a non-200 status code counts as down",
     spawn,
     {setup,
      fun () -> start_poller(), test_utils:start_http(2) end,
      fun stop_poller/1,
      fun (PidPortList) -> test_poll_hosts(PidPortList, [<<"/404">>, <<"/">>], [down, up]) end
     }
    }.


init_test()->
    {Status, {Aliases, timeout}} = poller_server:init(
                                     {alias_fixture(
                                        {9010, <<"/">>},
                                        {9020, <<"/foo">>}
                                       ),
                                      timeout
                                     }),
    ?assertEqual(
       {ok, [
             {<<"http://localhost:9010/">>, unknown, [{<<"spandex">>, <<"blue">>}]},
             {<<"http://localhost:9020/foo">>, unknown, [{<<"spandex">>, <<"green">>}]}
            ]},
       {Status, lists:sort(Aliases)}).
