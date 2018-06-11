-module(poller_server).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/2, start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([poll_hosts/2]).

start_link(AliasCfg, Timeout) ->
    gen_server:start_link({local, poller_srv}, ?MODULE, {AliasCfg, Timeout}, []).

start(AliasCfg, Timeout) ->
    %% ?debugFmt("poller_server:start(~p)~n", [{AliasCfg, Timeout}]),
    gen_server:start({local, poller_srv}, ?MODULE, {AliasCfg, Timeout}, []).

stop() ->
    gen_server:call(poller_srv, terminate).


poll_host({URL, _State, Aliases}, Timeout) ->
    {Host, Port, Path} = parse_url(URL),
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
                                      down
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
    [poll_host(Host, Timeout) | poll_hosts(Tail, Timeout)].

%%% Private API


parse_url(URL) ->
    %% ?debugFmt("parse_url: ~p", [URL]),
    [Schema, Rest1] = binary:split(URL, <<"://">>),
    [HostPort, Path] = case binary:split(Rest1, <<"/">>) of
                           [HostPort0] ->  [HostPort0, <<"">>];
                           [HostPort0, Path0] ->  [HostPort0, Path0]
                       end,
    %% {Host, Port} = route_spec_server:parse_host(HostPort),
    {Host, Port} = case binary:split(HostPort, <<":">>) of
                       [H] -> case Schema of
                                  <<"http">> -> {erlang:binary_to_list(H), 80};
                                  <<"https">> -> {erlang:binary_to_list(H), 443}
                              end;
                       [H, P] -> {erlang:binary_to_list(H), erlang:binary_to_integer(P)}
                   end,
    {Host, Port, <<$/, Path/binary>>}.


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
    {ok, {parse_aliases(Aliases), PollTimeout}}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

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

parse_url_test_() ->
    [
     ?_assertEqual({"example.com", 80, <<"/">>}, parse_url(<<"http://example.com">>)),
     ?_assertEqual({"example.com", 80, <<"/">>}, parse_url(<<"http://example.com/">>)),
     ?_assertEqual({"example.com", 80, <<"/foo/bar/">>}, parse_url(<<"http://example.com/foo/bar/">>)),
     ?_assertEqual({"example.com", 80, <<"/foo/bar">>}, parse_url(<<"http://example.com/foo/bar">>)),
     ?_assertEqual({"example.com", 443, <<"/foo/bar">>}, parse_url(<<"https://example.com/foo/bar">>))
    ].

-endif.
