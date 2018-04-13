-module(feature_flag_proxy_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init(Args) ->
    #{<<"route_spec_server">> := RouteCfg} = Args,
    Procs = [
             {route_srv,
              {route_spec_server, start_link, [RouteCfg]},
              permanent,
              100,
              worker,
              [route_spec_server]}
            ],
    {ok, {{one_for_one, 100, 5}, Procs}}.
