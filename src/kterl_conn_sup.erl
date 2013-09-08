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

-module(kterl_conn_sup).

-behaviour(supervisor).

-export([init/1, start_link/3]).
                  
start_link(ConnId, Host, Port) ->   
    supervisor:start_link({local, ConnId}, ?MODULE, [Host, Port]).

init([Host, Port]) ->
    MFA = {kterl_client, start_link, [Host, Port]},
    CS = {ignored, MFA, temporary, 5000, worker, [kterl_client]},
    {ok, {{simple_one_for_one, 3, 10}, [CS]}}.
