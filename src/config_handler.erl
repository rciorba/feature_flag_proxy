-module(config_handler).
-behavior(cowboy_handler).

-export([init/2]).



init(Req0, State) ->
    io:format("config~n"),
    Body = #{<<"routes">> =>
                 [
                   #{
                     <<"regexp">> => Regexp,
                     <<"host">> => <<
                                     (erlang:list_to_binary(Host))/binary,
                                     <<":">>/binary,
                                     (erlang:integer_to_binary(Port))/binary
                                   >>,
                     <<"enabled">> => Enabled,
                     <<"id">> => Id
                    }
                   || {spec, Regexp, {Host, Port}, Enabled, Id} <- route_spec_server:get_routespecs()]},
    Req1 = cowboy_req:reply(
             200,
             #{<<"content-type">> => <<"application/json">>},
             jiffy:encode(Body),
             Req0),
    {ok, Req1, State}.
