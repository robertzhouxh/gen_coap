#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/gen_coap/ebin/

main([]) ->
%   query("coap://coap.me:5683/.well-known/core").
    query("coap://127.0.0.1:5683/.well-known/core").

query(Uri) ->
    {ok, content, Data} = coap_client:request(get, Uri),
    Res = core_link:decode(Data),
    io:format("~p~n", [Res]).

% end of file
