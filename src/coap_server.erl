%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% CoAP server application
% supervisor for content registry, listening socket and channel supervisor
-module(coap_server).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).

-export([start_udp/1, start_udp/2, stop_udp/1, stop_udp/2]).
-export([start_dtls/2, start_dtls/3, stop_dtls/1, stop_dtls/2]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-include("coap.hrl").

-define(APP, gen_coap).

start() ->
    application:start(?APP).

stop() ->
    application:stop(?APP).

%% Application callbacks
start(normal, []) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

start_udp(Name) ->
    start_udp(Name, ?DEFAULT_COAP_PORT).

start_udp(Name, Port) ->
    MFA = {coap_channel_sup, start_channel, []},
    Opts = application:get_env(?APP, udp_options, []),
    start_child(esockd:udp_child_spec(Name, Port, Opts, MFA)).

stop_udp(Name) ->
    stop_udp(Name, ?DEFAULT_COAP_PORT).

stop_udp(Name, Port) ->
    stop_server(Name, Port).

start_dtls(Name, DtlsOpts) ->
    start_dtls(Name, ?DEFAULT_COAPS_PORT, DtlsOpts).

start_dtls(Name, Port, DtlsOpts) ->
    {ok, _} = application:ensure_all_started(ssl),
    Opts = merge_opts(application:get_env(?APP, dtls_options, []), DtlsOpts),
    MFA = {coap_channel_sup, start_channel, []},
    start_child(esockd:dtls_child_spec(Name, Port, [{dtls_options, Opts}], MFA)).

stop_dtls(Name) ->
    stop_dtls(Name, ?DEFAULT_COAPS_PORT).

stop_dtls(Name, Port) ->
    stop_server(Name, Port).

start_child(Spec) ->
    supervisor:start_child(?MODULE, Spec).

stop_server(Name, Port) ->
    ChildId = esockd_sup:child_id(Name, Port),
    case supervisor:terminate_child(?MODULE, ChildId) of
        ok    -> supervisor:delete_child(?MODULE, ChildId);
        Error -> Error
    end.

merge_opts(Defaults, Options) ->
    lists:foldl(fun({Opt, Val}, Acc) ->
                    lists:keystore(Opt, 1, Acc, {Opt, Val});
                   (Opt, Acc) ->
                    lists:usort([Opt | Acc])
                end, Defaults, Options).

%% Supervisor callbacks
init([]) ->
    {ok, {{one_for_all, 10, 100},
          [#{id       => coap_server_registry,
             start    => {coap_server_registry, start_link, []},
             restart  => permanent,
             shutdown => 5000,
             type     => worker,
             modules  => [coap_server_registry]},
           #{id       => coap_responder_sup,
             start    => {coap_responder_sup, start_link, []},
             restart  => permanent,
             shutdown => infinity,
             type     => supervisor,
             modules  => [coap_responder_sup]},
           #{id       => coap_channel_sup,
             start    => {coap_channel_sup, start_link, []},
             restart  => permanent,
             shutdown => infinity,
             type     => supervisor,
             modules  => [coap_channel_sup]}]}}.

