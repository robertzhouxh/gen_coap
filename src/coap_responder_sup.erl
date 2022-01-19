%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_responder_sup).
-behaviour(supervisor).

-export([start_link/0, get_responder/2, init/1]).

-include("coap.hrl").

start_link() ->
    supervisor:start_link(?MODULE, []).

get_responder(SupPid, Request) ->
    ?GLD_LOG("---> get responder for Request: ~p ~n", [Request]),
    case start_responder(SupPid, Request) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> 
	    ?GLD_LOG("---> pid=~p already-started ~n", [Pid]),
	    {ok, Pid};
        {error, Other} -> 
	    ?GLD_LOG("---> start err:~p~n", [Other]),
	    {error, Other}
    end.

start_responder(SupPid, #coap_message{method=_Method, options=Options}) ->
    Uri = proplists:get_value(uri_path, Options, []),
    [Query] = proplists:get_value(uri_query, Options, []),
    [DevId] = get_meta(Uri, Query),
    ?GLD_LOG("---> try to start coap_responder for devid: ~s with Uri: ~s in SupPid: ~p <<< OriginDevId: ~p, OriginUri: ~p >>> ~n~n", [DevId, Uri, SupPid, DevId, Uri]),
    supervisor:start_child(SupPid,
        {DevId,
            {coap_responder, start_link, [self(), Uri]},
            temporary, 5000, worker, []}).

init([]) ->
    pg:start_link(),
    {ok, {{one_for_one, 3, 10}, []}}.

get_meta([<<"mqtt">>, <<"auth">>], Query) -> 
    ?GLD_LOG("---> get_meta for auth, query=~s ~n", [Query]),
    Qry = binary:split(Query,<<$&>>,[global]),
    get_auth(Qry, []);
get_meta([<<"mqtt">>, <<"sys">>, _ModelId, DevId, _Action|_], _) -> [DevId];
get_meta([_|_], Query) -> 
    ?GLD_LOG("---> invalid uri just use query=~p ~n", [Query]),
    [Query].


get_auth([], Acc) -> Acc;
get_auth([<<$u, $=, Rest/binary>>|_], Acc) -> 
    U = cow_uri:urldecode(Rest),
    case binary:split(U,<<$&>>,[global]) of
	[DevId,_PrdId,_Ts] -> 
	    [DevId|Acc];
	_ -> 
	    ?GLD_LOG("-> can not find DevId for u=~s ~n", [U]),
	    Acc
    end;
get_auth([_|T], Acc) ->
    get_auth(T, Acc).


% end of file
