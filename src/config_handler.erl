-module(config_handler).
-behavior(cowboy_handler).

-export([init/2]).


init(Req0, State) ->
    io:format("config~n"),
    Req1 = cowboy_req:reply(
             200,
             #{<<"content-type">> => <<"text/plain">>},
             <<"Hello World">>,
             Req0),
    {ok, Req1, State}.
