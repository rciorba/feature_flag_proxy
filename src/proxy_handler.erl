-module(proxy_handler).
-behavior(cowboy_handler).

-export([init/2]).


receive_data(ConnPid, MRef, StreamRef) ->
    Data = receive_data(ConnPid, MRef, StreamRef, []),
    iolist_to_binary(lists:reverse(Data)).

receive_data(ConnPid, MRef, StreamRef, Accumulator) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_data(ConnPid, MRef, StreamRef, [Data | Accumulator]);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            [Data | Accumulator];
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        Any ->
            io:format("rd: ~p~n", [Any])
    after 10000 ->
        exit(timeout)
    end.


do_request(ConnPid, MRef, Path, Method, RequestHeaders) ->
    io:format("~p~n", [Method]),
    StreamRef = gun:request(ConnPid, Method, Path, RequestHeaders),
    io:format("StreamRef:~p~n", [StreamRef]),
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
            io:format("dr-nofin:~p~n", [Status]),
            {ok, Status, Headers, <<"">>};
        {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
            {ok, Status, Headers, receive_data(ConnPid, MRef, StreamRef)};
        {gun_error, ConnPid, StreamRef, Error} ->
            error_logger:error_msg("Oops!"),
            exit(Error);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        Any ->
            io:format("nomatch in dr: ~p~n", [Any])
    after 10000 ->
        exit(timeout)
    end.


cowboy_to_gun_headers(Map) ->
    [{K, binary:bin_to_list(V)} || {K, V} <- maps:to_list(Map),
                                   K =/= <<"connection">>
    ].


init(Req0, State) ->
    {ok, ConnPid} = gun:open("localhost", 9090),
    {ok, Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    Path = iolist_to_binary(
             [<<"/">> | lists:join(<<"/">>, cowboy_req:path_info(Req0))]),
    io:format("~p~n", [Path]),
    RequestHeaders = cowboy_to_gun_headers(maps:get(headers, Req0)),
    io:format("~p~n", [RequestHeaders]),
    {ok, Status, OriginalHeaders, Response} = do_request(
                                                ConnPid, 
                                                MRef, 
                                                Path,
                                                maps:get(method, Req0),
                                                RequestHeaders),
    Headers = [{<<"via">>, <<"1.1 feature_flag_proxy">>} | OriginalHeaders],
    Req = cowboy_req:reply(Status,
        %% #{<<"content-type">> => <<"text/html">>},
        maps:from_list(Headers),
        Response,
        Req0),
    gun:shutdown(ConnPid),
    {ok, Req, State}.
