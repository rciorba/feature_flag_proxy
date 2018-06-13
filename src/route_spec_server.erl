-module(route_spec_server).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start/1, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([disable_routespec/1, enable_routespec/1, get_routespecs/0,
         route_spec/2, route_spec/3, match_server/2, update_alias/2, update_active/1]).

-record(spec, {regexp, host, enabled=true, methods, id}).

start(Args) ->
    Success = gen_server:start({local, route_srv}, ?MODULE, Args, []),
    io:format("SL: ~p~n -> ~p~n", [Args, Success]),
    Success.

start_link(Args) ->
    Success = gen_server:start_link({local, route_srv}, ?MODULE, Args, []),
    io:format("SL: ~p~n -> ~p~n", [Args, Success]),
    Success.

stop() ->
    gen_server:call(route_srv, terminate).


copy_alias_key(Alias, ParsedAlias, Key) ->
    BinKey = erlang:atom_to_binary(Key, utf8),
    Hosts = maps:get(hosts, ParsedAlias),
    case maps:get(BinKey, Alias, null) of
        null ->
            %% nothing to copy
            ?debugFmt("no key ~p in:~n~p", [Key, Alias]),
            %% io:format("no key ~p in:~n~p~n", [Key, Alias]),
            ParsedAlias;
        Value ->
            case maps:is_key(Value, Hosts) of
                true -> maps:put(Key, Value, ParsedAlias);
                false -> exit({bad_host_alias, Key, Value, Alias})
            end
    end.

parse_alias(Alias) ->
    %% ?debugFmt("parse_alias:~p", [Alias]),
    ParseHosts = fun (_, Host) ->
                         parse_host(Host)
                 end,
    Parsed = #{
      hosts => maps:map(ParseHosts, maps:get(<<"hosts">>, Alias)),
      %% current => maps:get(<<"current">>, Alias),
      active => []
     },
    Parsed1 = copy_alias_key(Alias, Parsed, next_current),
    copy_alias_key(Alias, Parsed1, current).


parse_aliases(Aliases) ->
    %% ?debugFmt("parse_aliases:~p", [Aliases]),
    maps:map(fun(_, V)-> parse_alias(V) end, Aliases).

%%% Server functions
init(RouteCfg) ->
    io:format("INIT: ~p~n", [RouteCfg]),
    #{
      <<"routes">> := Routes,
      <<"default">> := Default
     } = RouteCfg,
    Aliases1 = maps:get(<<"blue-green">>, RouteCfg, #{}),
    Aliases2 = parse_aliases(Aliases1),
    io:format("Aliases:~p~n", [Aliases2]),
    RouteSpecs = [route_spec(Route) || Route <- Routes],
    {ok, {RouteSpecs, parse_host(Default), Aliases2}}.

handle_call(get, _From, State) ->
    {Routes, _Default, _Aliases} = State,
    {reply, Routes, State};
handle_call({match, Path, Method}, _From, State) ->
    %% ?debugFmt("handle_call:~p", {match, Path, Method}),
    {RouteSpecs, Default, Aliases} = State,
    {reply, match_server(Path, Method, RouteSpecs, Default, Aliases), State};
%% handle_call({add, RouteSpec}, _From, State) ->
%%     NewState = State ++ [RouteSpec],
%%     {reply, ok, NewState};
handle_call({disable, Id}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    case disable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default, Aliases}};
        badid ->
            %% io:format("~p~p~n", [Id, badid]),
            {reply, {error, erlang:iolist_to_binary(io_lib:format("No such id ~p", [Id]))}, State}
    end;
handle_call({enable, Id}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    case enable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default, Aliases}};
        badid -> {reply, error, State}
    end;
handle_call({update_alias, Alias, Host}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    case update_alias(Aliases, Alias, Host) of
        {ok, NewAliases} -> {reply, ok, {RouteSpecs, Default, transition_next_current(NewAliases)}};
        badhost -> {reply, error, State};
        badalias -> {reply, error, State}
    end;
handle_call({update_active, ActiveHosts}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    NewAliases = update_active(Aliases, ActiveHosts),
    {reply, ok, {RouteSpecs, Default, transition_next_current(NewAliases)}};
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


%% PrivateAPI

update_active(Aliases, ActiveHosts) ->
    error_logger:info_msg("Active hosts changed: ~p~n", [ActiveHosts]),
    maps:fold(
      fun (AliasName, AliasActiveHosts, Acc) ->
              Alias = maps:get(AliasName, Acc),
              NewAlias = maps:put(active, AliasActiveHosts, Alias),
              maps:put(AliasName, NewAlias, Acc)
      end,
      Aliases,
      ActiveHosts
    ).


update_alias(Aliases, AliasName, HostName) ->
    %% sets next_current
    case maps:get(AliasName, Aliases, badalias) of
        badalias -> badalias;
        Alias ->
            case maps:is_key(HostName, maps:get(hosts, Alias)) of
                false -> badhost;
                true ->
                    NewAlias = maps:put(next_current, HostName, Alias),
                    {ok, maps:put(AliasName, NewAlias, Aliases)}
            end
    end.

transition_next_current(Aliases) ->
    maps:map(
      fun(_, Alias) ->
              case maps:take(next_current, Alias) of
                  error -> Alias;
                  {NextCurrent, Alias1} ->
                      Active = maps:get(active, Alias1),
                      case lists:any(fun(E) -> E =:= NextCurrent end, Active) of
                          false -> Alias;
                          true -> maps:put(current, NextCurrent, Alias1)
                      end
              end
      end,
      Aliases).


%%% public API

disable_routespec(Id) ->
    gen_server:call(route_srv, {disable, Id}).

enable_routespec(Id) ->
    gen_server:call(route_srv, {enable, Id}).

get_routespecs() ->
    gen_server:call(route_srv, get).

match_server(Path, Method) ->
    gen_server:call(route_srv, {match, Path, Method}).

update_alias(Alias, Host) ->
    gen_server:call(route_srv, {update_alias, Alias, Host}).

update_active(ActiveHosts) ->
    gen_server:call(route_srv, {update_active, ActiveHosts}).

route_spec(Map) when is_map(Map) ->
    #{
      <<"regex">> := Regexp,
      <<"id">> := Id,
      <<"enabled">> := Enabled,
      <<"host">> := Host
     } = Map,
    Methods = maps:get(<<"methods">>, Map, any),
    route_spec(Regexp, parse_host(Host), Enabled, Id, Methods).

route_spec(Regexp, Host) ->
    route_spec(Regexp, Host, true).

route_spec(Regexp, Host, Enabled) ->
    route_spec(Regexp, Host, Enabled, rand:uniform(576460752303423487)).

route_spec(Regexp, Host, Enabled, Id) ->
    route_spec(Regexp, Host, Enabled, Id, any).

route_spec(Regexp, Host, Enabled, Id, Methods) ->
    #spec{regexp=Regexp, host=Host, enabled=Enabled, methods=Methods, id=Id}.

disable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, false).

enable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, true).


%%% Private API

parse_host(<< $$, HostBin/bits >>) ->
    %% io:format("================================ ~n", []),
    %% io:format("parse_host1: ~p~n", [HostBin]),
    %% io:format("================================ ~n", []),
    HostBin;
parse_host(URL) ->
    %% io:format("================================ ~n", []),
    %% io:format("parse_host2: ~p~n", [HostBin]),
    %% io:format("================================ ~n", []),
    {Host, Port, _Path} = requests:parse_url(URL),
    {Host, Port}.


resolve_alias(Host, _Aliases) when is_tuple(Host) ->
    Host;
resolve_alias(Host, Aliases) when is_binary(Host) ->
    case maps:get(Host, Aliases, badalias) of
        badalias -> badalias;
        Alias ->
            %% Active = maps:get(active, Alias, []),
            Hosts = maps:get(hosts, Alias),
            Current = maps:get(current, Alias),
            maps:get(Current, Hosts)
    end.

match_method(_Method, any)->
    true;
match_method(Method, Methods) ->
     lists:member(Method, Methods).


match_server(_Path, _Method, [], Default, _Aliases) ->
    Default;
match_server(Path, Method, RouteSpecs, Default, Aliases) ->
    [Spec | Tail] = RouteSpecs,
    %% io:format("Method:~p in ~p ?~n", [Method, Spec#spec.methods]),
    Match = case Spec#spec.enabled of
        true -> case re:run(Path, Spec#spec.regexp) of
                    {match, _} -> case match_method(Method, Spec#spec.methods) of
                                      true -> resolve_alias(Spec#spec.host, Aliases);
                                      _ -> null
                                  end;
                    nomatch -> null
                end;
        false -> null
    end,
    case Match of
        null -> match_server(Path, Method, Tail, Default, Aliases);
        _ -> Match
    end.


toggle_routespec(Id, RouteSpecs, Enabled) ->
    toggle_routespec(Id, RouteSpecs, Enabled, []).

toggle_routespec(_Id, [], _Enabled, _Acc) ->
    badid;
toggle_routespec(Id, RouteSpecs, Enabled, Acc) ->
    [Spec | Tail] = RouteSpecs,
    %% io:format("rs: ~p~n", [RouteSpecs]),
    case Spec#spec.id of
        Id -> New = Spec#spec{enabled = Enabled},
              Acc1 = [New | Acc],
              {ok, lists:reverse(Acc1) ++ Tail};
        _ -> toggle_routespec(Id, Tail, Enabled, [Spec | Acc])
    end.

-ifdef(EUNIT).

parse_aliases_test_() ->
    [
     ?_assertEqual(#{}, parse_aliases(#{})),
     ?_assertEqual(
        #{<<"A">> =>
              #{
                hosts =>
                    #{
                      <<"blue">> => {"blue", 9010},
                      <<"green">> => {"green", 9010}
                     },
                current => <<"blue">>,
                active => []
               }
         },
        parse_aliases(
          #{<<"A">> =>
                #{
                  <<"hosts">> =>
                      #{
                        <<"blue">> => <<"http://blue:9010/foo">>,
                        <<"green">> => <<"http://green:9010/foo">>
                       },
                  <<"current">> => <<"blue">>
                 }
           })
       )
    ].

update_alias_test_() ->
    AHost = #{
              hosts =>
                  #{
                    <<"blue">> => {"blue", 9010},
                    <<"green">> => {"green", 9010}
                   },
              current => <<"blue">>,
              active => []
             },
    InitialState = #{
                     <<"A">> => AHost
                    },
    [
     ?_assertEqual(
        {ok, maps:put(<<"A">>, maps:put(next_current, <<"green">>, AHost), InitialState)},
        update_alias(InitialState, <<"A">>, <<"green">>)),
     ?_assertEqual(
        badalias,
        update_alias(InitialState, <<"BadAlias">>, <<"green">>)),
     ?_assertEqual(
        badhost,
        update_alias(InitialState, <<"A">>, <<"turquoise">>))
    ].


transition_next_current_test_() ->
    NoActive = #{
                 <<"A">> => #{
                              hosts =>
                                  #{
                                    <<"blue">> => {"blue", 9010},
                                    <<"green">> => {"green", 9010}
                                   },
                              current => <<"green">>,
                              active => []
                             }
                },
    [
     ?_assertEqual(
        #{
          <<"A">> => #{
                       hosts =>
                           #{
                             <<"blue">> => {"blue", 9010},
                             <<"green">> => {"green", 9010}
                            },
                       current => <<"green">>,
                       active => [<<"green">>]
                      }
         },
        transition_next_current(
          #{
            <<"A">> => #{
                         hosts =>
                             #{
                               <<"blue">> => {"blue", 9010},
                               <<"green">> => {"green", 9010}
                              },
                         current => <<"blue">>,
                         next_current => <<"green">>,
                         active => [<<"green">>]
                        }
           })
       ),
     ?_assertEqual(NoActive, transition_next_current(NoActive))
    ].

update_active_test() ->
    AHost = #{
              hosts =>
                  #{
                    <<"blue">> => {"blue", 9010},
                    <<"green">> => {"green", 9010}
                   },
              current => <<"blue">>,
              active => []
             },
    InitialState = #{
                     <<"A">> => AHost
                    },
    ?assertEqual(maps:put(<<"A">>, maps:put(active, [<<"green">>], AHost), InitialState),
                 update_active(InitialState, #{<<"A">> => [<<"green">>]})).

-endif.
