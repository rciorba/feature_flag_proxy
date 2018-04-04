-module(feature_flag_proxy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
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
    ),
    feature_flag_proxy_sup:start_link().

stop(_State) ->
    ok.
