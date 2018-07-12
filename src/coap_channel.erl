%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% socket pair, identified by a 2-tuple of local and remote socket addresses
% stores state for a given endpoint
-module(coap_channel).

-behaviour(gen_server).

-export([start_link/2]).
-export([ping/1, send/2, send_request/3, send_message/3, send_response/3, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-define(VERSION, 1).
-define(IDLE_TIMEOUT, 2000).
-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-record(state, {mode, sock, cid, tokens, trans, nextmid, responders}).

-include("coap.hrl").

start_link(Scheme, ChId) when is_atom(Scheme) ->
    gen_server:start_link(?MODULE, [client, Scheme, ChId], []);

start_link(Transport, ChId) when is_tuple(Transport) ->
    gen_server:start_link(?MODULE, [server, Transport, ChId], []).

ping(Channel) ->
    send_message(Channel, make_ref(), #coap_message{type=con}).

send(Channel, Message=#coap_message{type=Type, method=Method})
        when is_tuple(Method); Type==ack; Type==reset ->
    send_response(Channel, make_ref(), Message);
send(Channel, Message=#coap_message{}) ->
    send_request(Channel, make_ref(), Message).

send_request(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_request, Message, {self(), Ref}}),
    {ok, Ref}.
send_message(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_message, Message, {self(), Ref}}),
    {ok, Ref}.
send_response(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_response, Message, {self(), Ref}}),
    {ok, Ref}.

close(Pid) ->
    gen_server:call(Pid, shutdown).

init([client, Scheme, ChId = {Host, Port}]) ->
    case coap_client_socket:connect(Scheme, Host, Port) of
        {ok, SockPid} ->
            {ok, init_state(client, SockPid, ChId)};
        {error, Reason} ->
            {stop, Reason}
    end;

init([server, {_, SockPid, _Sock}, ChId]) ->
    {ok, init_state(server, SockPid, ChId), ?IDLE_TIMEOUT}.

init_state(Mode, SockPid, ChId) ->
    process_flag(trap_exit, true),
    #state{mode = Mode, sock = SockPid, cid = ChId, tokens = #{}, trans = #{},
           responders = [], nextmid = first_mid()}.

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

% outgoing CON(0) or NON(1) request
handle_cast({send_request, Message, Receiver}, State) ->
    transport_new_request(Message, Receiver, State);
% outgoing CON(0) or NON(1)
handle_cast({send_message, Message, Receiver}, State) ->
    transport_new_message(Message, Receiver, State);
% outgoing response, either CON(0) or NON(1), piggybacked ACK(2) or RST(3)
handle_cast({send_response, Message, Receiver}, State) ->
    transport_response(Message, Receiver, State);
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:fwrite("coap_channel unknown cast ~p~n", [Request]),
    {noreply, State}.

transport_new_request(Message, Receiver, State=#state{tokens=Tokens}) ->
    Token = crypto:strong_rand_bytes(4), % shall be at least 32 random bits
    Tokens2 = maps:put(Token, Receiver, Tokens),
    transport_new_message(Message#coap_message{token=Token}, Receiver, State#state{tokens=Tokens2}).

transport_new_message(Message, Receiver, State=#state{nextmid=MsgId}) ->
    transport_message({out, MsgId}, Message#coap_message{id=MsgId}, Receiver, State#state{nextmid=next_mid(MsgId)}).

transport_message(TrId, Message, Receiver, State) ->
    update_state(State, TrId,
        coap_transport:send(Message, create_transport(TrId, Receiver, State))).

transport_response(Message=#coap_message{id=MsgId}, Receiver, State=#state{trans=Trans}) ->
    case maps:find({in, MsgId}, Trans) of
        {ok, TrState} ->
            case coap_transport:awaits_response(TrState) of
                true ->
                    update_state(State, {in, MsgId},
                        coap_transport:send(Message, TrState));
                false ->
                    transport_new_message(Message, Receiver, State)
            end;
        error ->
            transport_new_message(Message, Receiver, State)
    end.

% incoming CON(0) or NON(1) request
handle_info({datagram, SockPid, BinMessage= <<?VERSION:2, 0:1, _:1, _TKL:4, 0:3, _CodeDetail:5, MsgId:16, _/bytes>>},
            State = #state{sock = SockPid}) ->
    TrId = {in, MsgId},
    update_state(State, TrId,
        coap_transport:received(BinMessage, create_transport(TrId, undefined, State)));
% incoming CON(0) or NON(1) response
handle_info({datagram, SockPid, BinMessage= <<?VERSION:2, 0:1, _:1, TKL:4, _Code:8, MsgId:16, Token:TKL/bytes, _/bytes>>},
        State = #state{sock = SockPid, cid = ChId, tokens = Tokens, trans = Trans}) ->
    TrId = {in, MsgId},
    case maps:find(TrId, Trans) of
        {ok, TrState} ->
            update_state(State, TrId, coap_transport:received(BinMessage, TrState));
        error ->
            case maps:find(Token, Tokens) of
                {ok, Receiver} ->
                    update_state(State, TrId,
                        coap_transport:received(BinMessage, init_transport(TrId, Receiver, State)));
                error ->
                    % token was not recognized
                    BinReset = coap_packet:encode(#coap_message{type=reset, id=MsgId}),
                    io:fwrite("<- reset~n"),
                    SockPid ! {datagram, ChId, BinReset}
            end
    end;
% incoming ACK(2) or RST(3) to a request or response
handle_info({datagram, SockPid, BinMessage= <<?VERSION:2, _:2, _TKL:4, _Code:8, MsgId:16, _/bytes>>},
        State=#state{sock = SockPid, trans=Trans}) ->
    TrId = {out, MsgId},
    update_state(State, TrId,
        case maps:find(TrId, Trans) of
            error -> undefined; % ignore unexpected responses
            {ok, TrState} -> coap_transport:received(BinMessage, TrState)
        end);
% silently ignore other versions
handle_info({datagram, SockPid, <<Ver:2, _/bytes>>}, State = #state{sock = SockPid}) when Ver /= ?VERSION ->
    {noreply, State};
handle_info({timeout, TrId, Event}, State=#state{trans=Trans}) ->
    update_state(State, TrId,
        case maps:find(TrId, Trans) of
            error -> undefined; % ignore unexpected responses
            {ok, TrState} -> coap_transport:timeout(Event, TrState)
        end);
handle_info({request_complete, Token}, State=#state{tokens=Tokens}) ->
    Tokens2 = maps:remove(Token, Tokens),
    purge_state(State#state{tokens=Tokens2});
handle_info({responder_started, Pid}, State=#state{responders=Responders}) ->
    purge_state(State#state{responders = lists:usort([Pid|Responders])});
handle_info({responder_completed, Pid}, State=#state{responders=Responders}) ->
    purge_state(State#state{responders=lists:delete(Pid, Responders)});
handle_info({'EXIT', Pid, _Reason}, State = #state{responders = Responders}) ->
    {noreply, State#state{responders = lists:delete(Pid, Responders)}};
handle_info(timeout, State) ->
    {stop, idle_timeout, State};
handle_info(Info, State) ->
    io:fwrite("coap_channel: unexpected info: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{mode = client, sock=SockPid}) ->
    SockPid ! stop,
    ok;
terminate(Reason, #state{mode = server, responders = Responders}) ->
    lists:foreach(fun(Pid) -> coap_responder:shutdown(Pid, Reason) end, Responders).

first_mid() ->
    _ = rand:seed(exs1024),
    rand:uniform(?MAX_MESSAGE_ID).

next_mid(MsgId) ->
    if
        MsgId < ?MAX_MESSAGE_ID -> MsgId + 1;
        true -> 1 % or 0?
    end.

create_transport(TrId, Receiver, State=#state{trans=Trans}) ->
    case maps:find(TrId, Trans) of
        {ok, TrState} -> TrState;
        error -> init_transport(TrId, Receiver, State)
    end.

init_transport(TrId, Receiver, #state{sock=Sock, cid=ChId}) ->
    coap_transport:init(Sock, ChId, self(), TrId, Receiver).

update_state(State = #state{trans = Trans}, TrId, undefined) ->
    purge_state(State#state{trans = maps:remove(TrId, Trans)});
update_state(State = #state{trans = Trans}, TrId, TrState) ->
    {noreply, State#state{trans = maps:put(TrId, TrState, Trans)}}.

purge_state(State = #state{tokens=Tokens, trans=Trans, responders=Responders}) ->
    case maps:size(Tokens) + maps:size(Trans) + length(Responders)of
        0 -> {stop, normal, State};
        _Else -> {noreply, State}
    end.

