-module(poller_server).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/2, start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([poll_hosts/2, trigger_poll_hosts/0]).

start_link(AliasCfg, Timeout) ->
    gen_server:start_link({local, poller_srv}, ?MODULE, {AliasCfg, Timeout}, []).

start(AliasCfg, Timeout) ->
    %% ?debugFmt("poller_server:start(~p)~n", [{AliasCfg, Timeout}]),
    gen_server:start({local, poller_srv}, ?MODULE, {AliasCfg, Timeout}, []).

stop() ->
    gen_server:call(poller_srv, terminate).


poll_host({URL, _State, Aliases}, Timeout) ->
    {Host, Port, Path} = requests:parse_url(URL),
    NewState = case requests:open_connection({Host, Port}, Timeout) of
                   {ok, ConnPid, MRef} ->
                       Resp = case requests:do_request(
                                     ConnPid,
                                     MRef,
                                     Path,
                                     <<"GET">>,
                                     [
                                      {"Host", Host ++ ":" ++ erlang:integer_to_list(Port)}
                                     ]
                                    ) of
                                  {ok, 200, _Headers, _Body} ->
                                      %% ?debugFmt("~n~nup: ~p~n~p~n~n", [URL, {200, Headers, Body}]),
                                      up;
                                  {ok, _Status, _Headers, _Body} ->
                                      %% ?debugFmt("badstatus:~p", [{Status, Headers, Body}]),
                                      down;
                                  timeout ->
                                      down;
                                  Other ->
                                      ?debugFmt("~n >>>>>>>  BadReturn:~p", [Other]),
                                      throw({bad_return, Other})
                              end,
                       demonitor(MRef, [flush]),
                       gun:shutdown(ConnPid),
                       Resp;
                   {error, _Reason} ->
                       down
               end,
    {URL, NewState, Aliases}.

poll_hosts([], _Timeout) ->
    [];
poll_hosts([Host | Tail], Timeout) ->
    %% io:format("poll_hosts:~n~p~n", [[Host | Tail]]),
    [poll_host(Host, Timeout) | poll_hosts(Tail, Timeout)].

%%% Private API

get_active_aliases(State) ->
    UpHosts = lists:append(
                [
                 Aliases || {_, UpDown, Aliases} <- State,
                            UpDown =:= up
                ]),
    lists:foldl(
      fun({Alias, Host}, Acc) ->
              HostList = maps:get(Alias, Acc, []),
              maps:put(Alias, HostList++[Host], Acc)
      end,
      #{},
      UpHosts
     ).

trigger_poll_hosts() ->
    gen_server:cast(poller_srv, poll_and_update).

list_map_merge(Map1, Map2) ->
    maps:fold(
      fun(K, List1, Acc) ->
              List2 = maps:get(K, Acc, []),
              maps:put(K, lists:append(List1, List2), Acc)
      end,
      Map2,
      Map1).


parse_alias(Alias, AliasName) ->
    Hosts = maps:get(<<"hosts">>, Alias),
    Alias1 = maps:fold(
      fun(K, V, Acc) ->
              NameList = maps:get(V, Acc, []),
              maps:put(V, [{AliasName, K} | NameList], Acc)
      end,
      #{},
      Hosts),
    maps:map(fun(_, V)-> lists:sort(V) end, Alias1).


parse_aliases(Aliases) ->
    parse_aliases(maps:to_list(Aliases), #{}).

parse_aliases([{AliasName, Alias} | Tail], Accumulator)->
    %% ?debugFmt("~n===> {~p, ~p}~n~p", [AliasName, Alias, Accumulator]),
    NewAcc = list_map_merge(Accumulator, parse_alias(Alias, AliasName)),
    %% ?debugFmt("~n<=== ~p", [NewAcc]),
    parse_aliases(Tail, NewAcc);
parse_aliases([], Accumulator) ->
    [{K, unknown, V} || {K, V} <- maps:to_list(Accumulator)].


%%% Server functions
init({Aliases, PollTimeout}) when is_map(Aliases) ->
    %% ?debugFmt("poller_server:init(~p)~n", [Aliases]),
    {ok, _} = timer:apply_interval(timer:seconds(30), ?MODULE, trigger_poll_hosts, []),
    {ok, _} = timer:apply_after(100, ?MODULE, trigger_poll_hosts, []),
    {ok, {parse_aliases(Aliases), PollTimeout}}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(poll_and_update, State) ->
    {Aliases, Timeout} = State,
    %% io:format("cast:~n~p~n", [State]),
    NewAliases = poll_hosts(Aliases, Timeout),
    case NewAliases =:= Aliases of
        false ->
            ActiveHosts = get_active_aliases(NewAliases),
            route_spec_server:update_active(ActiveHosts),
            {noreply, {NewAliases, Timeout}};
        true ->
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("Terminate: ~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.


%%% Unit Tests

-ifdef(EUNIT).
parse_alias_test() ->
    SampleAlias = #{<<"hosts">> =>
                         #{
                           <<"blue">> => <<"http://localhost:9000">>,
                           <<"green">> => <<"http://localhost:9000">>
                          }
                   },
    ?assertEqual(
       #{<<"http://localhost:9000">> =>
             [
              {<<"spandex">>, <<"blue">>},
              {<<"spandex">>, <<"green">>}
             ]
        },
       parse_alias(SampleAlias, <<"spandex">>)
      ).

parse_aliases_test() ->
    SampleAliases =  #{
                       <<"A">> =>
                           #{<<"hosts">> =>
                                 #{
                                   <<"blue">> => <<"http://10">>,
                                   <<"green">> => <<"http://20/foo">>
                                  }
                            },
                       <<"B">> =>
                           #{<<"hosts">> =>
                                 #{
                                   <<"blue">> => <<"http://10">>,
                                   <<"green">> => <<"http://10">>
                                  }
                            }
                       },
    ?assertEqual(
       [
        {<<"http://10">>, unknown, [{<<"A">>, <<"blue">>},
                                    {<<"B">>, <<"blue">>},
                                    {<<"B">>, <<"green">>}]},
        {<<"http://20/foo">>, unknown, [{<<"A">>, <<"green">>}]}
       ],
       parse_aliases(SampleAliases)
      ).


list_map_merge_test_() ->
    [
     ?_assertEqual(#{}, list_map_merge(#{}, #{})),
     ?_assertEqual(#{a=> [1, 2]}, list_map_merge(#{a=>[1]}, #{a=>[2]})),
     ?_assertEqual(#{a=> [1, 2, 3], b=> [1]}, list_map_merge(#{a=>[1, 2], b=>[1]}, #{a=>[3]}))
    ].

get_active_aliases_test_() ->
    [
     ?_assertEqual(
        #{<<"A">> => [<<"blue">>], <<"B">> => [<<"green">>]},
        get_active_aliases([
                            {<<"http://10">>, up, [{<<"A">>, <<"blue">>},
                                                   {<<"B">>, <<"green">>}]},
                            {<<"http://20/foo">>, down, [{<<"A">>, <<"green">>}]}
                           ])
       ),
     ?_assertEqual(
        #{<<"A">> => [<<"blue">>, <<"green">>], <<"B">> => [<<"green">>]},
        get_active_aliases([
                            {<<"http://10">>, up, [{<<"A">>, <<"blue">>},
                                                   {<<"B">>, <<"green">>}]},
                            {<<"http://20/foo">>, up, [{<<"A">>, <<"green">>}]}
                           ])
       )
    ].


-endif.
