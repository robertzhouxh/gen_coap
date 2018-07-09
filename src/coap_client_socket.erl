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

-export([start_link/3, init/1, loop/1, stop/1]).

-record(state, {owner, scheme, sock, host, port}).

-define(TIMEOUT, 10000).

start_link(Scheme, Host, Port) ->
    start_link(Scheme, Host, Port, ?TIMEOUT).

start_link(Scheme, Host, Port, Timeout) ->
    proc_lib:start_link(?MODULE, init, [[self(), Scheme, Host, Port, Timeout]]).

init([Parent, Scheme, Host, Port, Timeout]) ->
    case connect(Scheme, Host, Port, Timeout) of
        {ok, Sock} ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(#state{owner = Parent, scheme = Scheme, sock = Sock, host = Host, port = Port});
        {error, Reason} ->
            proc_lib:init_ack(Parent, {error, Reason})
    end.

connect(coap, _Host, _Port, _Timeout) ->
    gen_udp:open(0, [binary, {active, true}, {reuseaddr, true}]);

connect(coaps, Host, Port, Timeout) ->
    Ciphers = ["ECDH-RSA-AES128-SHA","AES128-SHA"],
    ssl:connect(Host, Port, [binary, {protocol, dtls}, {ciphers, Ciphers}], Timeout).

loop(State = #state{owner = Owner, scheme = Scheme, sock = Sock}) ->
    receive
        %% Send ->
        {datagram, _To, Data} ->
            ok = send(Data, State),
            hibernate(State);
        %% Recv <-
        {udp, Sock, _PeerIP, _PeerPortNo, Data} ->
            Owner ! {datagram, self(), Data},
            hibernate(State);
        %% Recv <-
        {ssl, Sock, Data} ->
            Owner ! {datagram, self(), Data},
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

send(Data, #state{scheme = coap, sock = Sock, host = Host, port = Port}) ->
    gen_udp:send(Sock, Host, Port, Data);
send(Data, #state{scheme = coaps, sock = Sock}) ->
    ssl:send(Sock, Data).

hibernate(State) ->
    erlang:hibernate(?MODULE, loop, [State]).

close(coap, Sock)  -> gen_udp:close(Sock);
close(coaps, Sock) -> ssl:close(Sock).

