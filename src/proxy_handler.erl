-module(proxy_handler).
-behavior(cowboy_handler).

-export([init/2]).


receive_data(ConnPid, MRef, StreamRef) ->
    case receive_data(ConnPid, MRef, StreamRef, []) of
        timeout -> timeout;
        Data -> {ok, iolist_to_binary(lists:reverse(Data))}
    end.


receive_data(ConnPid, MRef, StreamRef, Accumulator) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_data(ConnPid, MRef, StreamRef, [Data | Accumulator]);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            [Data | Accumulator];
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        {gun_error, MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        Any ->
            io:format("rd: ~p~n", [Any]),
            exit(unexpected_msg)
    after 5000 ->
        timeout
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
            case receive_data(ConnPid, MRef, StreamRef) of
                {ok, Data} -> {ok, Status, Headers, Data};
                timeout -> timeout
            end;
        {gun_error, ConnPid, StreamRef, Error} ->
            error_logger:error_msg("Cacao!"),
            io:format("dr_error:~p~n", [Error]),
            exit(Error);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason);
        Any ->
            io:format("nomatch in dr: ~p~n", [Any])
    after 15000 ->
        timeout
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


open_connection({Host, Port}) ->
    {ok, ConnPid} = gun:open(Host, Port),
    case gun:await_up(ConnPid) of
        {ok, _Protocol} ->
            MRef = monitor(process, ConnPid),
            {ok, ConnPid, MRef};
        {error, Reason} -> {error, Reason}
    end.


error_response(Req, Status, ResponseBody) ->
    {Status, cowboy_req:reply(Status,
                              #{<<"content-length">> => integer_to_binary(byte_size(ResponseBody)),
                                <<"content-type">> => <<"text/plain">>},
                              ResponseBody,
                              Req)}.


proxy_request(Req, ConnPid, MRef, Path) ->
    RequestHeaders = transform_request_headers(maps:get(headers, Req)),
    {ok, Body, Req0} = case cowboy_req:has_body(Req) of
                           true -> cowboy_req:read_body(Req);
                           false -> {ok, null, Req}
                       end,
    Req1 = case do_request(
                  ConnPid, MRef, Path, maps:get(method, Req0), RequestHeaders, Body) of
               {ok, Status, OriginalHeaders, ResponseBody} ->
                   Headers = transform_response_headers(OriginalHeaders),
                   cowboy_req:reply(Status,
                                    maps:from_list(Headers),
                                    ResponseBody,
                                    Req0);
               timeout ->
                   Status = 504,
                   error_response(Req0, Status, <<"upstream timedout">>)
           end,
    gun:shutdown(ConnPid),
    {Status, Req1}.


init(Req, State) ->
    QString = cowboy_req:qs(Req),
    QStringList = case QString of
                      <<"">> -> [];
                      _ -> [<<"?">>, QString]
                  end,
    Path0 = lists:join(<<"/">>, cowboy_req:path_info(Req)) ++ QStringList,
    Path = iolist_to_binary([<<"/">> | Path0]),
    %% io:format("~p~n", [Path]),
    HostTuple = route_spec_server:match_server(Path),
    {Success, {Status, Req1}, Reason} = case open_connection(HostTuple) of
                                    {ok, ConnPid, MRef} ->
                                        {ok, proxy_request(
                                               Req, ConnPid, MRef, Path),
                                         null
                                        };
                                    {error, timeout} ->
                                        {error, error_response(
                                                  Req, 504, <<"Connection to upstream timed out.">>),
                                         timeout
                                        };
                                    {error, Reason1} ->
                                        {error, error_response(
                                                  Req, 500, erlang:atom_to_binary(Reason1, utf8)),
                                         Reason1
                                        }
                                end,
    case Success of
        ok ->
            io:format("OK: ~p - ~p - ~p~n", [HostTuple, Status, Path]);
        error ->
            io:format("Err: ~p - ~p [~p] - ~p~n", [HostTuple, Status, Reason, Path])
    end,
    {ok, Req1, State}.
