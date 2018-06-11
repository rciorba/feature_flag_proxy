-module(requests).

-export([open_connection/1, open_connection/2, do_request/5, do_request/6]).
-include_lib("eunit/include/eunit.hrl").



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


open_connection(HostPort) ->
    open_connection(HostPort, 5000).

open_connection({Host, Port}, Timeout) ->
    {ok, ConnPid} = gun:open(Host, Port),
    case gun:await_up(ConnPid, Timeout) of
        {ok, _Protocol} ->
            MRef = monitor(process, ConnPid),
            %% ?debugFmt("open_connection -> ~n~p", [{ok, ConnPid, MRef}]),
            {ok, ConnPid, MRef};
        {error, Reason} -> {error, Reason}
    end.

do_request(ConnPid, MRef, Path, Method, RequestHeaders) ->
    do_request(ConnPid, MRef, Path, Method, RequestHeaders, null).

do_request(ConnPid, MRef, Path, Method, RequestHeaders, Timeout) when is_integer(Timeout) ->
    do_request(ConnPid, MRef, Path, Method, RequestHeaders, null, Timeout);
do_request(ConnPid, MRef, Path, Method, RequestHeaders, Body) ->
    do_request(ConnPid, MRef, Path, Method, RequestHeaders, Body, 5000).

do_request(ConnPid, MRef, Path, Method, RequestHeaders, Body, Timeout) ->
    %% TODO: should probably use gun:await here for synchronous behavior
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
            io:format("dr_error:~p~n", [Error]),
            exit(Error);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("monitored gun process died!"),
            exit(Reason);
        Any ->
            io:format("nomatch in dr:~n~p~n~p~n", [Any, {MRef, ConnPid}])
    after Timeout ->
        timeout
    end.
