-module(feature_flag_proxy_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([Cfg]) ->
    io:format("SUP_INIT: ~p~n", [Cfg]),
    #{<<"route_spec_server">> := RouteCfg} = Cfg,
    Aliases = maps:get(<<"blue-green">>, RouteCfg, #{}),
    Procs = [
             {poller_srv,
              {poller_server, start_link, [Aliases, 5000]},
              permanent,
              100,
              worker,
              [poller_server]},
             {route_srv,
              {route_spec_server, start_link, [RouteCfg]},
              permanent,
              100,
              worker,
              [route_spec_server]}
            ],
    {ok, {{one_for_one, 100, 5}, Procs}}.
