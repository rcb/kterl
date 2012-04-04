%% Copyright (c) 2012, Rich Beerman <rbeer@beerman.org>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private

-module(kterl_request).
-author('rbeer@beerman.org').

-include("kterl.hrl").

-export([gen_request_iolist/4]).

-spec gen_request_iolist(
        Host     :: string() | binary(), 
        Reqtype  :: binary(),
        Body     :: iolist(),
        Bodylen  :: non_neg_integer()) -> iolist().

gen_request_iolist(Host, Reqtype, Body, Bodylen) ->
    [gen_header(Host, Reqtype, Bodylen) | Body].

gen_header(Host, Reqtype, Bodylen) ->
    Clen = case Bodylen of
               0 -> <<"Content-Length: 0">>;
               _ -> [<<"Content-Length: ">>, integer_to_list(Bodylen)]
           end,

    [<<"POST /rpc/">>, Reqtype, <<" HTTP/1.1\r\n">>,
     <<"Host: ">>, Host, <<"\r\n">>,
     <<"Content-Type: text/tab-separated-values; colenc=B\r\n">>,
     Clen, <<"\r\n\r\n">>].

