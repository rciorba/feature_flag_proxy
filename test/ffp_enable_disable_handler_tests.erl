-module(ffp_enable_disable_handler_tests).
-include_lib("eunit/include/eunit.hrl").


ensure_all_started([App|Apps]) ->
    application:ensure_all_started(App),
    ensure_all_started(Apps);
ensure_all_started([]) ->
    ok.

sample_config(Routes) ->
    #{<<"route_spec_server">> =>
          #{<<"routes">> => Routes,
            <<"default">> => <<"http://example.com">>
           }
     }.

start() ->
    ok = ensure_all_started([cowboy, jiffy, gun]),
    {_InetsPid, Port} = test_utils:start_http(),
    Routes = [
              test_utils:fake_route(
                #{<<"regex">> => <<"/foo">>,
                  <<"host">> =>
                      << <<"http://127.0.0.1:">>/binary, (erlang:integer_to_binary(Port))/binary >>}
               )
             ],
    {ok, Pid} = feature_flag_proxy_app:start(unused_start_type, [sample_config(Routes)]),
    {ok, ConnPid, MRef} = requests:open_connection({"127.0.0.1", 8081}, 5),
    {Pid, ConnPid, MRef}.

stop({_Pid, _ConnPid, MRef}) ->
    demonitor(MRef, [flush]),
    application:stop(feature_flag_proxy).

ignore_headers(timeout) ->
    timeout;
ignore_headers({ok, Status, _Headers, Data}) ->
    {ok, Status, Data}.


test_default_routes({_Pid, ConnPid, MRef}) ->
    [
     ?_assertEqual(
        {ok, 200, <<"some dummy content here">>},
        ignore_headers(
          requests:do_request(ConnPid, MRef, <<"/foo">>, <<"GET">>, [], 1200)))
    ].

spam_test_()->
    {spawn,
     {setup,
      fun start/0,
      fun stop/1,
      fun test_default_routes/1
     }}.
