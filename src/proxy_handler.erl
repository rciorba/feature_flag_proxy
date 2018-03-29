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
            io:format("rd: ~p~n", [Any]),
            exit(unexpected_msg)
    after 10000 ->
        exit(timeout)
    end.


do_request(ConnPid, MRef, Path, Method, RequestHeaders, Body) ->
    %% io:format("Method:~p~nPath:~p~nBody:~p~nHeaders:~p~n", [Method, Path, Body, RequestHeaders]),
    StreamRef = case Body of
                    null -> gun:request(ConnPid, Method, Path, RequestHeaders);
                    _ -> gun:request(ConnPid, Method, Path, RequestHeaders, Body)
                end,
    %% io:format("StreamRef:~p~n", [StreamRef]),
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
            %% io:format("dr-nofin:~p~n", [Status]),
            {ok, Status, Headers, <<"">>};
        {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
            {ok, Status, Headers, receive_data(ConnPid, MRef, StreamRef)};
        {gun_error, ConnPid, StreamRef, Error} ->
            error_logger:error_msg("Cacao!"),
            io:format("dr_error:~p~n", [Error]),
            exit(Error);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        Any ->
            io:format("nomatch in dr: ~p~n", [Any])
    after 10000 ->
        exit(timeout)
    end.


transform_request_headers(Map) ->
    [{K, binary:bin_to_list(V)} ||
        {K, V} <- filter_proxy_headers(maps:to_list(Map))
    ].


filter_proxy_headers(Headers) ->
    [{Name, Value} || {Name, Value} <- Headers,
        Name =/= <<"connection">>,
        Name =/= <<"content-length">>,
        Name =/= <<"te">>,
        Name =/= <<"transfer-encoding">>,
        Name =/= <<"keep-alive">>,
        Name =/= <<"proxy-authorization">>,
        Name =/= <<"proxy-authentication">>,
        Name =/= <<"trailer">>,
        Name =/= <<"upgrade">>
    ].

fix_header_name(HeaderName) ->
    case HeaderName of
        <<"set-cookie">> -> <<"Set-Cookie">>;
        Any -> Any
    end.


transform_response_headers(Headers) ->
    %% TODO: https://www.mnot.net/blog/2011/07/11/what_proxies_must_do
    [{<<"via">>, <<"1.1 feature_flag_proxy">>} |
     [{fix_header_name(K), V} || {K, V} <- filter_proxy_headers(Headers)]
    ].


open_connection(Path) ->
    {Host, Port} = route_spec_server:match_server(Path),
    io:format("~p - ~p~n", [{Host, Port}, Path]),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    {ok, ConnPid, MRef}.


init(Req, State) ->
    QString = cowboy_req:qs(Req),
    QStringList = case QString of
                      <<"">> -> [];
                      _ -> [<<"?">>, QString]
                  end,
    Path0 = lists:join(<<"/">>, cowboy_req:path_info(Req)) ++ QStringList,
    Path = iolist_to_binary([<<"/">> | Path0]),
    %% io:format("~p~n", [Path]),
    {ok, ConnPid, MRef} = open_connection(Path),
    RequestHeaders = transform_request_headers(maps:get(headers, Req)),
    %% io:format("RequestHeaders: ~p~n", [RequestHeaders]),
    {ok, Body, Req0} = case cowboy_req:has_body(Req) of
                           true -> cowboy_req:read_body(Req);
                           false -> {ok, null, Req}
                       end,
    {ok, Status, OriginalHeaders, ResponseBody} = do_request(
                                                ConnPid,
                                                MRef,
                                                Path,
                                                maps:get(method, Req0),
                                                RequestHeaders,
                                                Body),
    Headers = transform_response_headers(OriginalHeaders),
    %% io:format("ResponseHeaders: ~p~nStatus:~p~nBody:~p~n", [Headers, Status, ResponseBody]),
    Req1 = cowboy_req:reply(Status,
        %% #{<<"content-type">> => <<"text/html">>},
        maps:from_list(Headers),
        ResponseBody,
        Req0),
    gun:shutdown(ConnPid),
    {ok, Req1, State}.
