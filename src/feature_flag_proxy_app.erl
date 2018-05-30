-module(feature_flag_proxy_app).
-behaviour(application).

%% -compile(export_all).
-export([start/2]).
-export([stop/1]).


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



start(_Type, _Args) ->
    {ok, _Pid} = feature_flag_proxy_sup:start_link(read_config()),
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
