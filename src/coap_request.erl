%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
% Copyright (c) 2018 Feng Lee <feng@emqx.io>
%

-module(coap_request).

-include("coap.hrl").

-export([token/1, method/1, uri_path/1, uri_query/1]).

token(#coap_message{token = Token}) ->
    Token.

method(#coap_message{method = Method}) ->
    Method.

uri_path(#coap_message{options = Options}) ->
    proplists:get_value(uri_path, Options, []).

uri_query(#coap_message{options = Options}) ->
    proplists:get_value(uri_query, Options, []).

