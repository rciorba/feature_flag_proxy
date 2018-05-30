-module(poller_server_tests).
-include_lib("eunit/include/eunit.hrl").

alias_fixture() ->
    alias_fixture(9010, 9020).
alias_fixture(BluePort, GreenPort) ->
    BPort = integer_to_binary(BluePort),
    GPort = integer_to_binary(GreenPort),
    #{<<"spandex">> =>
          #{<<"hosts">> =>
                #{
                  <<"blue">> => <<"http://localhost:", BPort/binary>>,
                  <<"green">> => <<"http://localhost:", GPort/binary>>
                 }
           }
     }.

start_http() ->
    application:start(inets),
    {ok, CWD} = file:get_cwd(),
    Path = lists:append(CWD, "/test/fake_http"),
    ?debugFmt("~p", [Path]),
    {ok, Pid} = inets:start(httpd, [{port, 0},
                                    {server_name,"httpd_test"}, {server_root,CWD},
                                    {document_root,Path}, {bind_address, "localhost"}]),
    {port, Port} = lists:keyfind(port, 1, httpd:info(Pid)),
    {Pid, Port}.

stop_http({Pid, _Port}) ->
    inets:stop(httpd, Pid).

start_blue_green() ->
    %% inets:start(),
    {BluePid, BluePort} = start_http(),
    {GreenPid, GreenPort} = start_http(),
    ?debugFmt("~p, ~p", [{BluePid, BluePort}, {GreenPid, GreenPort}]),
    {{BluePid, BluePort}, {GreenPid, GreenPort}}.


stop_blue_green({Blue, Green}) ->
    %% gen_server:call(terminate, Pid).
    %% inets:stop(),
    Got = {stop_http(Blue), stop_http(Green)},
    ?debugFmt("stop_blue_green:~p", [Got]).


start_poller() ->
    {ok, _} = application:ensure_all_started(gun),
    {{BluePid, BluePort}, {GreenPid, GreenPort}} = start_blue_green(),
    {ok, _} = poller_server:start(alias_fixture(BluePort, GreenPort)),
    {{BluePid, BluePort}, {GreenPid, GreenPort}}.

stop_poller(BlueGreen) ->
    stop_blue_green(BlueGreen),
    poller_server:stop().


test_poll_hosts({{_, BluePort}, {_, GreenPort}}) ->
    {ok, State} = poller_server:init(alias_fixture(BluePort, GreenPort)),
    ExpectedState = [{URL, up, Aliases} || {URL, _, Aliases} <- State],
    ?_assertEqual(ExpectedState, poller_server:poll_hosts(State)).


polling_happy_path_test_()->
    {setup,
     fun start_poller/0,
     fun stop_poller/1,
     fun test_poll_hosts/1
    }.


init_test()->
    {Status, State} = poller_server:init(alias_fixture()),
    ?assertEqual(
       {ok, [
             {<<"http://localhost:9010">>, unknown, [{<<"spandex">>, <<"blue">>}]},
             {<<"http://localhost:9020">>, unknown, [{<<"spandex">>, <<"green">>}]}
            ]},
       {Status, lists:sort(State)}).
