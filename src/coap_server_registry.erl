%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% registry of server content handlers
-module(coap_server_registry).

-behaviour(gen_server).

-export([start_link/0]).
-export([add_handler/3, get_handler/1, del_handler/2, get_links/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {}).

-define(TAB, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_handler(Prefix, Module, Args) ->
    gen_server:call(?MODULE, {add_handler, Prefix, Module, Args}).

del_handler(Prefix, Module) ->
    gen_server:call(?MODULE, {del_handler, Prefix, Module}).

get_handler(Uri) ->
    unwrap(ets:foldl(
             fun(Elem = {{Prefix, _}, _}, Found) ->
                 case lists:prefix(Prefix, Uri) of
                     true -> one_with_longer_uri(Elem, Found);
                     false -> Found
                 end
             end, undefined, ?TAB)).

get_links() ->
    ets:foldl(
        fun({{Prefix, Module}, Args}, Acc) ->
            Acc ++ get_links(Prefix, Module, Args)
        end, [], ?TAB).

unwrap({{Prefix, Module}, Args}) ->
    {Prefix, Module, Args};
unwrap(undefined) ->
    undefined.

init(_Args) ->
    _ = ets:new(?TAB, [ordered_set, protected, named_table, {read_concurrency, true}]),
    % RFC 6690, Section 4
    true = ets:insert(?TAB, {{[<<".well-known">>, <<"core">>], coap_server_content}, undefined}),
    {ok, #state{}}.

handle_call({add_handler, Prefix, Module, Args}, _From, State) ->
    true = ets:insert(?TAB, {{Prefix, Module}, Args}),
    {reply, ok, State};

handle_call({del_handler, Prefix, Module}, _From, State) ->
    true = ets:delete(?TAB, {Prefix, Module}),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    error_logger:error_msg("[~s] Unexpected request: ~p", [?MODULE, Req]),
    {reply, ignore, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("[~s] Unexpected msg: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("[~s] Unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

% select an entry with a longest prefix
% this allows user to have one handler for "foo" and another for "foo/bar"
one_with_longer_uri(Elem1, undefined) -> Elem1;
one_with_longer_uri(Elem1 = {{Prefix, _}, _}, {{Match, _}, _}) when length(Prefix) > length(Match) -> Elem1;
one_with_longer_uri(_Elem1, Elem2) -> Elem2.

% ask each handler to provide a link list
get_links(Prefix, Module, Args) ->
    case erlang:function_exported(Module, coap_discover, 2) of
        % for each pattern ask the handler to provide a list of resources
        true -> apply(Module, coap_discover, [Prefix, Args]);
        false -> []
    end.

