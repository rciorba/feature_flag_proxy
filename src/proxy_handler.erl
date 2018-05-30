-module(proxy_handler).
-behavior(cowboy_handler).

-export([init/2]).


host_tuple_to_string({Host, Port}) ->
    Host ++ ":" ++ erlang:integer_to_list(Port).


transform_request_headers(Map, HostTuple) ->
    Headers = [{K, binary:bin_to_list(V)} ||
                  {K, V} <- filter_proxy_headers(maps:to_list(Map))
              ],
    [ {"Host", host_tuple_to_string(HostTuple)} | Headers].


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
        Name =/= <<"upgrade">>,
        Name =/= <<"host">>
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


error_response(Req, Status, ResponseBody) ->
    {Status, cowboy_req:reply(Status,
                              #{<<"content-length">> => integer_to_binary(byte_size(ResponseBody)),
                                <<"content-type">> => <<"text/plain">>},
                              ResponseBody,
                              Req)}.


proxy_request(Req, ConnPid, MRef, Path, HostTuple) ->
    RequestHeaders = transform_request_headers(maps:get(headers, Req), HostTuple),
    {ok, Body, Req0} = case cowboy_req:has_body(Req) of
                           true -> cowboy_req:read_body(Req);
                           false -> {ok, null, Req}
                       end,
    {Status1, Req1} = case requests:do_request(
                           ConnPid, MRef, Path, maps:get(method, Req0), RequestHeaders, Body) of
                        {ok, Status, OriginalHeaders, ResponseBody} ->
                            Headers = transform_response_headers(OriginalHeaders),
                            {Status,
                             cowboy_req:reply(Status,
                                              maps:from_list(Headers),
                                              ResponseBody,
                                              Req0)};
                        timeout ->
                            Status = 504,
                            {Status,
                             error_response(Req0, Status, <<"upstream timedout">>)}
                      end,
    gun:shutdown(ConnPid),
    {Status1, Req1}.


init(Req, State) ->
    QString = cowboy_req:qs(Req),
    QStringList = case QString of
                      <<"">> -> [];
                      _ -> [<<"?">>, QString]
                  end,
    Path0 = lists:join(<<"/">>, cowboy_req:path_info(Req)) ++ QStringList,
    Path = iolist_to_binary([<<"/">> | Path0]),
    Method = maps:get(method, Req),
    %% io:format("~p~n", [Path]),
    HostTuple = route_spec_server:match_server(Path, Method),
    {Success, {Status, Req1}, Reason} = case requests:open_connection(HostTuple) of
                                    {ok, ConnPid, MRef} ->
                                        {ok, proxy_request(
                                               Req, ConnPid, MRef, Path, HostTuple),
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
            io:format("OK: ~p - ~p - ~p ~p~n", [HostTuple, Status, Path, Method]);
        error ->
            io:format("Err: ~p - ~p [~p] - ~p ~p~n", [HostTuple, Status, Reason, Path, Method])
    end,
    {ok, Req1, State}.
