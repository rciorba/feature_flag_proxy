-module(config_handler).
-behavior(cowboy_handler).

-export([init/2]).


dump_host(Host) when is_binary(Host)->
    <<$$, Host/binary>>;
dump_host({Host, Port}) ->
    <<
      (erlang:list_to_binary(Host))/binary,
      <<":">>/binary,
      (erlang:integer_to_binary(Port))/binary
    >>.


init(Req0, State) ->
    io:format("config~n"),
    Body = #{
             <<"routes">> =>
                 [
                   #{
                     <<"regexp">> => Regexp,
                     <<"host">> => dump_host(Host),
                     <<"enabled">> => Enabled,
                     <<"methods">> => Methods,
                     <<"id">> => Id
                    }
                   || {spec, Regexp, Host, Enabled, Methods, Id}
                          <- route_spec_server:get_routespecs()
                 ]
            },
    Req1 = cowboy_req:reply(
             200,
             #{<<"content-type">> => <<"application/json">>},
             jiffy:encode(Body),
             Req0),
    {ok, Req1, State}.
