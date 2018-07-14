%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
% Copyright (c) 2018 Feng Lee <feng@emqx.io>
%

-module(coap_client_socket).

-include("coap.hrl").

-export([connect/3, connect/4, connect/5]).
-export([init/1, loop/1, stop/1]).

-record(state, {owner, scheme, sock, host, port}).

-define(TIMEOUT, 10000).

connect(Scheme, Host, Port) ->
    connect(Scheme, Host, Port, []).
connect(Scheme, Host, Port, Options) ->
    connect(Scheme, Host, Port, Options, ?TIMEOUT).
connect(Scheme, Host, Port, Options, Timeout) ->
    proc_lib:start_link(?MODULE, init, [[self(), Scheme, Host, Port, Options, Timeout]]).

init([Parent, Scheme, Host, Port, Options, Timeout]) ->
    case do_connect(Scheme, Host, Port, Options, Timeout) of
        {ok, Sock} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(#state{owner = Parent, scheme = Scheme, sock = Sock, host = Host, port = Port});
        {error, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason})
    end.

do_connect(coap, _Host, _Port, Options, _Timeout) ->
    gen_udp:open(0, lists:ukeymerge(1, [{active, true}, {mode, binary}, {reuseaddr, true}], Options));
do_connect(coaps, Host, Port, Options, Timeout) ->
    ssl:connect(Host, Port, lists:ukeymerge(1, [{mode, binary}, {protocol, dtls}], Options), Timeout).

loop(State = #state{owner = Owner, scheme = Scheme, sock = Sock}) ->
    receive
        %% Send ->
        {datagram, _To, Packet} ->
            ok = send(Packet, State),
            hibernate(State);
        %% Recv <-
        {udp, Sock, _PeerIP, _PeerPortNo, Packet} ->
            Owner ! {datagram, self(), Packet},
            hibernate(State);
        %% Recv <-
        {ssl, Sock, Packet} ->
            Owner ! {datagram, self(), Packet},
            hibernate(State);
        {ssl_error, Sock, Reason} ->
            exit({ssl_error, Reason});
        {ssl_closed, Sock} ->
            exit(ssl_closed);
        stop ->
            close(Scheme, Sock);
        Info ->
            io:format("Info: ~p~n", [Info])
    end.

stop(SockPid) when is_pid(SockPid) ->
    SockPid ! stop.

send(Packet, #state{scheme = coap, sock = Sock, host = Host, port = Port}) ->
    gen_udp:send(Sock, Host, Port, Packet);
send(Packet, #state{scheme = coaps, sock = Sock}) ->
    ssl:send(Sock, Packet).

hibernate(State) ->
    erlang:hibernate(?MODULE, loop, [State]).

close(coap, Sock)  -> gen_udp:close(Sock);
close(coaps, Sock) -> ssl:close(Sock).

