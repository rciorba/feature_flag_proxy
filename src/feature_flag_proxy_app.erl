-module(feature_flag_proxy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


parse_config(Data) ->
    jiffy:decode(Data, [return_maps]).


read_config() ->
    case os:getenv("FFP_CONFIG") of
        false -> exit("FFP_CONFIG not set");
        CfgPath -> read_config(CfgPath)
    end.


read_config(CfgPath) ->
    case file:read_file(CfgPath) of
        {ok, Data} -> parse_config(Data);
        {error, Reason} ->
            io:format("Error ~p. Could not load config from ~p~n", [Reason, CfgPath]),
            exit("Could not load config file")
    end.


%% type_of(Value) ->
%%     case is_list()


%% validate_config(Config, ExpectedSchema) ->
%%     validate_config(Config, ExpectedSchema, [], []).

%% validate_config(RouteCfg, ExpectedSchema, Errors, Prefixes) ->
%%     {Key, Value} = map:take(ExpectedSchema),
%%     RouteCfg = map:get(Key, null),
%%     case RouteCfg of
%%         null -> throw(<<"Bad config, missing key ">> + Key);
%%         _ -> error
%%     end.


make_full_key(Prefix, Key) ->
    StrKey = binary_to_list(Key),
    case Prefix of
        "" -> StrKey;
        _ -> string:join([Prefix, StrKey], ".")
    end.

map_check(Key, Map, Prefix, CheckerFun) ->
    FullKey = make_full_key(Prefix, Key),
    Value = case maps:get(Key, Map, null) of
        null ->
            throw({bad_config, string:concat("Missing key ", FullKey)});
        Val -> Val
    end,
    case CheckerFun(Value) of
        true ->
            ok;
        _ ->
            TypeName = type_name(Value),
            throw({bad_config, string:join(["Wrong type", TypeName, "for key", FullKey], " ")})
    end,
    Value.


type_name(Val) when is_binary(Val) ->
    "binary";
type_name(Val) when is_list(Val) ->
    "list";
type_name(Val) when is_map(Val) ->
    "map";
type_name(Val)  when is_tuple(Val) ->
    "tuple";
type_name(Val)  when is_boolean(Val) ->
    "boolean";
type_name(Val)  when is_integer(Val) ->
    "integer";
type_name(Val)  when is_float(Val) ->
    "float";
type_name(Val) when is_atom(Val) ->
    "atom";
type_name(_Val) ->
    "unknown_type".


validate_routes_config([], _Count) ->
    ok;
validate_routes_config([Route|Routes], Count) ->
    Prefix = string:concat("route_spec_server.routes.", integer_to_list(Count)),
    map_check(<<"id">>, Route, Prefix, fun is_integer/1),
    map_check(<<"regex">>, Route, Prefix, fun is_binary/1),
    map_check(<<"enabled">>, Route, Prefix, fun is_boolean/1),
    map_check(<<"host">>, Route, Prefix, fun is_binary/1),
    validate_routes_config(Routes, Count+1).


validate_routes_config(Routes) ->
    validate_routes_config(Routes, 0).


validate_config(Cfg) ->
    RouteSpecSrv = map_check(<<"route_spec_server">>, Cfg, "", fun is_map/1),
    Routes = map_check(<<"routes">>, RouteSpecSrv, "route_spec_server", fun is_list/1),
    validate_routes_config(Routes),
    map_check(<<"default">>, RouteSpecSrv, "route_spec_server", fun is_binary/1),
    ok.


start(StartType, []) ->
    Cfg = read_config(),
    io:format("CFG: ~p~n", [Cfg]),
    start(StartType, [Cfg]);
start(_StartType, [Cfg]) ->
    validate_config(Cfg),
    io:format("CFG: ~p~n", [Cfg]),
    {ok, _Pid} = feature_flag_proxy_sup:start_link([Cfg]),
    Dispatch = cowboy_router:compile([
    %%% {HostMatch, list({PathMatch, Handler, InitialState})}
        {
         '_', [
               {"/_config/", config_handler, []},
               {"/_config/:routeid", ffp_enable_disable_handler, []},
               {"/[...]", proxy_handler, []}
              ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ).

stop(_State) ->
    ok.

%%% Unit Tests

-ifdef(EUNIT).

fake_alias(Args) ->
    #{<<"hosts">>
          => #{<<"blue">> => maps:get(<<"blue">>, Args, <<"http://blue.example.com">>),
               <<"green">> => maps:get(<<"green">>, Args, <<"http://green.example.com">>)
              },
      <<"current">> => maps:get(<<"current">>, Args, <<"blue">>)
     }.

fake_alias() ->
    fake_alias(#{}).

validate_config_test_() ->
    %% ExpectedSchema = #{<<"route_spec_server">>
    %%                        => #{
    %%                             <<"routes">> => list,
    %%                             <<"default">> => string,
    %%                             <<"blue-green">> => {optional, #{}}
    %%                            }
    %%                   },
    [
     ?_assertEqual(
        ok,
        validate_config(
          #{<<"route_spec_server">> => #{<<"routes">> =>[
                                                         test_utils:fake_route(#{<<"id">> => 10}),
                                                         test_utils:fake_route(#{<<"id">> => 20})
                                                        ],
                                         <<"default">> => <<"http://example.com/">>,
                                         <<"blue-green">> => #{<<"alias1">> => fake_alias()}
                                        }
           })),
     ?_assertThrow(
        {bad_config, "Missing key route_spec_server"},
        validate_config(#{})),
     ?_assertThrow(
        {bad_config, "Missing key route_spec_server.routes"},
        validate_config(#{<<"route_spec_server">> => #{}})),
     ?_assertThrow(
        {bad_config, "Wrong type list for key route_spec_server"},
        validate_config(#{<<"route_spec_server">> => []})),
    ?_assertThrow(
        {bad_config, "Wrong type map for key route_spec_server.routes"},
        validate_config(#{<<"route_spec_server">> => #{ <<"routes">>=>#{} }})),
    ?_assertThrow(
        {bad_config, "Missing key route_spec_server.routes.0.id"},
        validate_config(#{<<"route_spec_server">> => #{ <<"routes">>=>[#{}] }})),
    ?_assertThrow(
        {bad_config, "Wrong type list for key route_spec_server.routes.1.id"},
        validate_config(#{<<"route_spec_server">>
                              => #{ <<"routes">>
                                        =>[ test_utils:fake_route(), %% a valid route
                                            #{<<"id">>=>"foo"}  %% bad id
                                          ] }})),
    ?_assertThrow(
        {bad_config, "Missing key route_spec_server.default"},
        validate_config(
          #{<<"route_spec_server">> => #{<<"routes">> =>[]}})),
    ?_assertThrow(
        {bad_config, "Wrong type atom for key route_spec_server.default"},
        validate_config(
          #{<<"route_spec_server">> => #{<<"routes">> =>[], <<"default">> => not_a_binary_string}}))
    ].


validate_routes_config_test_()->
    [
     ?_assertEqual(ok, validate_routes_config([test_utils:fake_route()])),
     ?_assertThrow(
        {bad_config, "Missing key route_spec_server.routes.0.id"},
        validate_routes_config([#{}])
      ),
     ?_assertThrow(
        {bad_config, _},
        validate_routes_config([#{<<"id">> => 10}])
      ),
     ?_assertThrow(
        {bad_config, _},
        validate_routes_config([#{<<"id">> => 10, <<"regex">> => <<"">>}])
      ),
     ?_assertThrow(
        {bad_config, _},
        validate_routes_config([#{<<"id">> => 10, <<"regex">> => <<"">>, <<"enabled">> => true}])
      )
    ].

-endif.
