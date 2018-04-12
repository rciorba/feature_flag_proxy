-module(feature_flag_proxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
             {route_srv,
              {route_spec_server, start_link, []},
              permanent,
              1,
              worker,
              [route_spec_server]}
            ],
    {ok, {{one_for_one, 100, 5}, Procs}}.
