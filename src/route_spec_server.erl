-module(route_spec_server).
-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([disable_routespec/1, enable_routespec/1, get_routespecs/0,
         route_spec/2, route_spec/3, match_server/1]).

-record(spec, {regexp, host, enabled=true, id}).

start_link(Args) ->
    %% io:format("SL: ~p~n", [Args]),
    gen_server:start_link({local, route_srv}, ?MODULE, Args, []).

stop() ->
    gen_server:call(route_srv, terminate).

%%% Server functions
init(RouteCfg) ->
    %% io:format("INIT: ~p~n", [RouteCfg]),
    #{<<"routes">> := Routes, <<"default">> := Default} = RouteCfg,
    RouteSpecs = [route_spec(Route) || Route <- Routes],
    {ok, {RouteSpecs, parse_host(Default)}}. %% no treatment of info here!

handle_call(get, _From, State) ->
    {Routes, _Default} = State,
    {reply, Routes, State};
handle_call({match, Path}, _From, State) ->
    {RouteSpecs, Default} = State,
    {reply, match_server(Path, RouteSpecs, Default), State};
handle_call({add, RouteSpec}, _From, State) ->
    NewState = State ++ [RouteSpec],
    {reply, ok, NewState};
handle_call({disable, Id}, _From, State) ->
    {RouteSpecs, Default} = State,
    case disable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default}};
        badid ->
            %% io:format("~p~p~n", [Id, badid]),
            {reply, {error, erlang:iolist_to_binary(io_lib:format("No such id ~p", [Id]))}, State}
    end;
handle_call({enable, Id}, _From, State) ->
    {RouteSpecs, Default} = State,
    case enable_routespec(Id, RouteSpecs) of
        {ok, NewRouteSpecs} -> {reply, ok, {NewRouteSpecs, Default}};
        badid -> {reply, error, {RouteSpecs, Default}}
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

match_server(Path) ->
    gen_server:call(route_srv, {match, Path}).

route_spec(Map) when is_map(Map) ->
    #{
      <<"regex">> := Regexp,
      <<"id">> := Id,
      <<"enabled">> := Enabled,
      <<"host">> := Host
     } = Map,
    route_spec(Regexp, parse_host(Host), Enabled, Id).

route_spec(Regexp, {Host, Port}) ->
    route_spec(Regexp, {Host, Port}, true).

route_spec(Regexp, {Host, Port}, Enabled) ->
    route_spec(Regexp, {Host, Port}, Enabled, rand:uniform(576460752303423487)).

route_spec(Regexp, {Host, Port}, Enabled, Id) ->
    #spec{regexp=Regexp, host={Host, Port}, enabled=Enabled, id=Id}.

disable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, false).

enable_routespec(Id, RouteSpecs) ->
    toggle_routespec(Id, RouteSpecs, true).


%%% Private API

parse_host(HostBin) ->
    [Schema, HostPort] = binary:split(HostBin, <<"://">>),
    case binary:split(HostPort, <<":">>) of
        [Host] -> case Schema of
                      <<"http">> -> {erlang:binary_to_list(Host), 80};
                      <<"https">> -> {erlang:binary_to_list(Host), 443}
                  end;
        [Host, Port] -> {erlang:binary_to_list(Host), erlang:binary_to_integer(Port)}
    end.


match_server(_Path, [], Default) ->
    Default;
match_server(Path, RouteSpecs, Default) ->
    [Spec | Tail] = RouteSpecs,
    case Spec#spec.enabled of
        true -> case re:run(Path, Spec#spec.regexp) of
                    {match, _} -> Spec#spec.host;
                    nomatch -> match_server(Path, Tail, Default)
                end;
        false -> match_server(Path, Tail, Default)
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
