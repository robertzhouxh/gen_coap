%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% stores one channel handler per endpoint
% when communication ceases the respective channel exits normally
-module(coap_channel_sup).

-behaviour(supervisor).

-export([start_link/0, start_channel/2, count_channels/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel(SockPid, ChId) ->
    supervisor:start_child(?MODULE, [SockPid, ChId]).

count_channels() ->
    proplists:get_value(active, supervisor:count_children(?MODULE), 0).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [#{id       => channel,
             start    => {coap_channel, start_link, []},
             restart  => transient,
             shutdown => 5000,
             type     => worker,
             modules  => [coap_channel]}]}}.

