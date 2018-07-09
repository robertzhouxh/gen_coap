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

-export([start_link/0, get_responder/3, start_responder/3, count_responders/0]).

-export([init/1]).

-include("coap.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_responder(Channel, ChId, Request) ->
    case start_responder(Channel, ChId, Request) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, Other} -> {error, Other}
    end.

start_responder(Channel, ChId, Request) ->
    Method = coap_request:method(Request),
    Uri = coap_request:uri_path(Request),
    Query = coap_request:uri_query(Request),
    supervisor:start_child(?MODULE,
                           #{id      => {Channel, ChId, {Method, Uri, Query}},
                            start    => {coap_responder, start_link, [Channel, ChId, Request]},
                            restart  => temporary,
                            shutdown => 5000,
                            type     => worker,
                            modules  => [coap_responder]}).

count_responders() ->
    proplists:get_value(active, supervisor:count_children(?MODULE), 0).

init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.

