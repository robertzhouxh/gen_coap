#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/gen_coap/ebin -pa _build/default/lib/esockd/ebin

main(_Params) ->
    sample_server:start(),
    receive stop->ok end.
