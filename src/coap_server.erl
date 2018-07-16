%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% CoAP server application
-module(coap_server).

-behaviour(application).
-behaviour(supervisor).

-include("coap.hrl").

-export([start/0, stop/0]).
-export([start_udp/1, start_udp/2,  start_udp/3, stop_udp/1, stop_udp/2]).
-export([start_dtls/1, start_dtls/2, start_dtls/3, stop_dtls/1, stop_dtls/2]).
-export([add_handler/3, del_handler/2]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-define(APP, gen_coap).

start() ->
    application:ensure_all_started(?APP).

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
    start_udp(Name, Port, []).

start_udp(Name, Port, Opts) ->
    UdpOpts = merge_opts(application:get_env(?APP, udp_options, []), Opts),
    start_server(fun esockd:udp_child_spec/4, Name, Port, [{udp_options, UdpOpts}]).

stop_udp(Name) ->
    stop_udp(Name, ?DEFAULT_COAP_PORT).

stop_udp(Name, Port) ->
    stop_server(Name, Port).

start_dtls(Name) ->
    start_dtls(Name, ?DEFAULT_COAPS_PORT).

start_dtls(Name, Port) ->
    start_dtls(Name, Port, []).

start_dtls(Name, Port, Opts) ->
    {ok, _} = application:ensure_all_started(ssl),
    DtlsOpts = merge_opts(application:get_env(?APP, dtls_options, []), Opts),
    start_server(fun esockd:dtls_child_spec/4, Name, Port, [{dtls_options, DtlsOpts}]).

stop_dtls(Name) ->
    stop_dtls(Name, ?DEFAULT_COAPS_PORT).

stop_dtls(Name, Port) ->
    stop_server(Name, Port).

start_server(SpecFun, Name, Port, Opts) ->
    MFA = {coap_channel_sup, start_channel, []},
    supervisor:start_child(?MODULE, SpecFun(Name, Port, Opts, MFA)).

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

add_handler(Prefix, Module, Args) ->
    coap_server_registry:add_handler(Prefix, Module, Args).

del_handler(Prefix, Module) ->
    coap_server_registry:del_handler(Prefix, Module).

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

