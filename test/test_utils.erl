-module(test_utils).

-include_lib("eunit/include/eunit.hrl").
-export([flush_inbox/0, start_http/0, start_http/1, stop_http/1, fake_route/0, fake_route/1]).

flush_inbox() ->
    receive
        Any ->
            ?debugFmt("~nflushing :~p~n", [Any]),
            ok
    after 0 ->
            ok
    end.

fake_route(Args) ->
    #{
      <<"id">> => maps:get(<<"id">>, Args, 10),
      <<"host">> => maps:get(<<"host">>, Args, <<"http://127.0.0.1/">>),
      <<"regex">> => maps:get(<<"regex">>, Args, <<"^/fake/">>),
      <<"enabled">> => maps:get(<<"enabled">>, Args, true)
     }.

fake_route() ->
    fake_route(#{}).


start_http() ->
    application:start(inets),
    {ok, CWD} = file:get_cwd(),
    Path = lists:append(CWD, "/test/fake_http"),
    %% ?debugFmt("~p", [Path]),
    {ok, Pid} = inets:start(httpd, [{port, 0},
                                    {server_name,"httpd_test"}, {server_root,CWD},
                                    {document_root,Path}, {bind_address, "localhost"}]),
    {port, Port} = lists:keyfind(port, 1, httpd:info(Pid)),
    {Pid, Port}.

start_http(0) ->
    [];
start_http(N) when N >= 0 ->
    [start_http()] ++ start_http(N-1).

stop_http([]) ->
    ok;
stop_http([{null, _Port} | Tail]) ->
    stop_http(Tail);
stop_http([{Pid, _Port} | Tail]) ->
    ok = inets:stop(httpd, Pid),
    stop_http(Tail).
