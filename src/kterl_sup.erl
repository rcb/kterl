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

%% @doc
%%
%% This is kterl app's main supervisor. 
%% When kterl:connect is called, a new kterl_conn_sup child supervisor is 
%% started for each unique {host, port} server pair. The individual tcp
%% client connection gen_servers (kterl_client) are then linked as children
%% of the appropriate kterl_conn_sup supervisor:
%%
%%        [kterl_sup supervisor]
%%                   |
%%  -----------------|-------------------------------
%% |                                                |
%% [kterl_conn_sup: 127.0.0.1:1978]     [127.0.0.1:1980]
%% |        |     |      |     |       |    |    |     |
%% pid1  pid2   pid3   pid4 ...
%%

-module(kterl_sup).

-behaviour(supervisor).

-export([init/1, start_link/0, new_connection/3]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

-spec new_connection(
        Host :: string(),
        Port :: inet:port_number(),
        ReconnectSleep :: non_neg_integer()) -> {ok, pid()} | {error, term()}.

new_connection(Host, Port, ReconnectSleep) ->
    ConnId = conn_id(Host, Port),
    MFA = {kterl_conn_sup, start_link, [ConnId, Host, Port]},
    CS = {ConnId, MFA, permanent, 5000, supervisor, [kterl_conn_sup]},
    ConnSupPid = extract_supervisor_pid(
                   supervisor:start_child(?MODULE, CS)),
    supervisor:start_child(ConnSupPid, [ReconnectSleep]).

extract_supervisor_pid({ok, Pid}) -> Pid;
extract_supervisor_pid({error, {already_started, Pid}}) -> Pid.

conn_id(Host, Port) ->
    list_to_atom(Host ++ ":" ++ integer_to_list(Port)).
