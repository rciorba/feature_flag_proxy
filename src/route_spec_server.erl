-module(route_spec_server).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start/1, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([disable_routespec/1, enable_routespec/1, get_routespecs/0,
         route_spec/2, route_spec/3, match_server/2]).

-record(spec, {regexp, host, enabled=true, methods, id}).

start(Args) ->
    Success = gen_server:start({local, route_srv}, ?MODULE, Args, []),
    io:format("SL: ~p~n -> ~p~n", [Args, Success]),
    Success.

start_link(Args) ->
    Success = gen_server:start_link({local, route_srv}, ?MODULE, Args, []),
    io:format("SL: ~p~n -> ~p~n", [Args, Success]),
    Success.

stop() ->
    gen_server:call(route_srv, terminate).

parse_alias(Alias) ->
    %% ?debugFmt("parse_alias:~p", [Alias]),
    ParseHosts = fun (_, Host) ->
                         parse_host(Host)
                 end,
    #{
      hosts => maps:map(ParseHosts, maps:get(<<"hosts">>, Alias)),
      current => maps:get(<<"current">>, Alias)
     }.
parse_aliases(Aliases) ->
    %% ?debugFmt("parse_aliases:~p", [Aliases]),
    maps:map(fun(_, V)-> parse_alias(V) end, Aliases).

%%% Server functions
init(RouteCfg) ->
    io:format("INIT: ~p~n", [RouteCfg]),
    #{
      <<"routes">> := Routes,
      <<"default">> := Default
     } = RouteCfg,
    Aliases1 = maps:get(<<"blue-green">>, RouteCfg, #{}),
    Aliases2 = parse_aliases(Aliases1),
    %% ?debugFmt("pa:~p", [Aliases2]),
    RouteSpecs = [route_spec(Route) || Route <- Routes],
    {ok, {RouteSpecs, parse_host(Default), Aliases2}}. %% no treatment of info here!

handle_call(get, _From, State) ->
    {Routes, _Default, _Aliases} = State,
    {reply, Routes, State};
handle_call({match, Path, Method}, _From, State) ->
    %% ?debugFmt("handle_call:~p", {match, Path, Method}),
    {RouteSpecs, Default, Aliases} = State,
    {reply, match_server(Path, Method, RouteSpecs, Default, Aliases), State};
%% handle_call({add, RouteSpec}, _From, State) ->
%%     NewState = State ++ [RouteSpec],
%%     {reply, ok, NewState};
handle_call({disable, Id}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    case disable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default, Aliases}};
        badid ->
            %% io:format("~p~p~n", [Id, badid]),
            {reply, {error, erlang:iolist_to_binary(io_lib:format("No such id ~p", [Id]))}, State}
    end;
handle_call({enable, Id}, _From, State) ->
    {RouteSpecs, Default, Aliases} = State,
    case enable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default, Aliases}};
        badid -> {reply, error, {RouteSpecs, Default, Aliases}}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("Terminate: ~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.


%%% public API

disable_routespec(Id) ->
    gen_server:call(route_srv, {disable, Id}).

enable_routespec(Id) ->
    gen_server:call(route_srv, {enable, Id}).

get_routespecs() ->
    gen_server:call(route_srv, get).

match_server(Path, Method) ->
    gen_server:call(route_srv, {match, Path, Method}).

route_spec(Map) when is_map(Map) ->
    #{
      <<"regex">> := Regexp,
      <<"id">> := Id,
      <<"enabled">> := Enabled,
      <<"host">> := Host
     } = Map,
    Methods = maps:get(<<"methods">>, Map, any),
    route_spec(Regexp, parse_host(Host), Enabled, Id, Methods).

route_spec(Regexp, Host) ->
    route_spec(Regexp, Host, true).

route_spec(Regexp, Host, Enabled) ->
    route_spec(Regexp, Host, Enabled, rand:uniform(576460752303423487)).

route_spec(Regexp, Host, Enabled, Id) ->
    route_spec(Regexp, Host, Enabled, Id, any).

route_spec(Regexp, Host, Enabled, Id, Methods) ->
    #spec{regexp=Regexp, host=Host, enabled=Enabled, methods=Methods, id=Id}.

disable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, false).

enable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, true).


%%% Private API

parse_host(<< $$, HostBin/bits >>) ->
    io:format("================================ ~n", []),
    io:format("parse_host1: ~p~n", [HostBin]),
    io:format("================================ ~n", []),
    HostBin;
parse_host(HostBin) ->
    io:format("================================ ~n", []),
    io:format("parse_host2: ~p~n", [HostBin]),
    io:format("================================ ~n", []),
    [Schema, HostPort] = binary:split(HostBin, <<"://">>),
    case binary:split(HostPort, <<":">>) of
        [Host] -> case Schema of
                      <<"http">> -> {erlang:binary_to_list(Host), 80};
                      <<"https">> -> {erlang:binary_to_list(Host), 443}
                  end;
        [Host, Port] -> {erlang:binary_to_list(Host), erlang:binary_to_integer(Port)}
    end.


resolve_alias(Host, _Aliases) when is_tuple(Host) ->
    Host;
resolve_alias(Host, Aliases) when is_binary(Host) ->
    case maps:get(Host, Aliases, badalias) of
        badalias ->
            badalias;
        Alias ->
            %% Active = maps:get(active, Alias, []),
            Hosts = maps:get(hosts, Alias),
            Current = maps:get(current, Alias),
            maps:get(Current, Hosts)
    end.

match_method(_Method, any)->
    true;
match_method(Method, Methods) ->
     lists:member(Method, Methods).


match_server(_Path, _Method, [], Default, _Aliases) ->
    Default;
match_server(Path, Method, RouteSpecs, Default, Aliases) ->
    [Spec | Tail] = RouteSpecs,
    %% io:format("Method:~p in ~p ?~n", [Method, Spec#spec.methods]),
    Match = case Spec#spec.enabled of
        true -> case re:run(Path, Spec#spec.regexp) of
                    {match, _} -> case match_method(Method, Spec#spec.methods) of
                                      true -> resolve_alias(Spec#spec.host, Aliases);
                                      _ -> null
                                  end;
                    nomatch -> null
                end;
        false -> null
    end,
    case Match of
        null -> match_server(Path, Method, Tail, Default, Aliases);
        _ -> Match
    end.


toggle_routespec(Id, RouteSpecs, Enabled) ->
    toggle_routespec(Id, RouteSpecs, Enabled, []).

toggle_routespec(_Id, [], _Enabled, _Acc) ->
    badid;
toggle_routespec(Id, RouteSpecs, Enabled, Acc) ->
    [Spec | Tail] = RouteSpecs,
    %% io:format("rs: ~p~n", [RouteSpecs]),
    case Spec#spec.id of
        Id -> New = Spec#spec{enabled = Enabled},
              Acc1 = [New | Acc],
              {ok, lists:reverse(Acc1) ++ Tail};
        _ -> toggle_routespec(Id, Tail, Enabled, [Spec | Acc])
    end.
