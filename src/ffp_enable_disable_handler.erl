-module(ffp_enable_disable_handler).
-behavior(cowboy_handler).

-export([init/2]).


to_integer(Binary) ->
    case string:to_integer(Binary) of
        {Int, <<"">>} ->
            {ok, Int};
        _ -> error
    end.


to_boolean(Binary) ->
    Bin0 = string:lowercase(Binary),
    case lists:member(Bin0, [<<"true">>, <<"1">>]) of
        true -> true;
        false -> case lists:member(Bin0, [<<"false">>, <<"0">>]) of
                     true -> false;
                     _ -> error
                 end
    end.


fmt(FString, Args)->
    erlang:iolist_to_binary(
      io_lib:format(FString, Args)).


update_route_spec(RouteId, Enabled) when is_binary(RouteId)->
    case to_integer(RouteId) of
        {ok, Int} -> update_route_spec(Int, Enabled);
        _ -> {error, fmt("Bad RouteId: '~p' is not an integer", [RouteId])}
    end;
update_route_spec(RouteId, Enabled) when is_integer(RouteId)->
    case to_boolean(Enabled) of
        true -> route_spec_server:enable_routespec(RouteId);
        false -> route_spec_server:disable_routespec(RouteId);
        error -> {error, fmt("Bad value '~p' for argument enabled", [Enabled])}
    end.


post(Req0, State) ->
    io:format("enable/disable~n"),
    #{enabled := Enabled} = cowboy_req:match_qs([{enabled, nonempty, false}], Req0),
    RouteId = cowboy_req:binding(routeid, Req0),
    Result = update_route_spec(RouteId, Enabled),
    Req1 = case Result of
        ok ->
            cowboy_req:reply(
              200,
              #{<<"content-type">> => <<"application/json">>},
              jiffy:encode(#{<<"status">> => <<"ok">>}),
              Req0);
        {error, Reason} ->
            cowboy_req:reply(
              400,
              #{<<"content-type">> => <<"application/json">>},
              jiffy:encode(#{<<"status">> => <<"error">>,
                             <<"message">> => Reason}),
              Req0)
    end,
    {ok, Req1, State}.


init(Req, State) ->
    Req1 = case maps:get(method, Req) of
        <<"POST">> ->
            post(Req, State);
        BadMethod ->
            cowboy_req:reply(
              405,
              #{<<"content-type">> => <<"application/json">>},
              jiffy:encode(#{<<"status">> => <<"error">>,
                             <<"message">> => fmt("Method ~p not allowed", [BadMethod])}),
              Req)
    end,
    {ok, Req1, State}.
