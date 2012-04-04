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
%% kterl_client is a non-blocking gen_server based tcp connection
%% handler, roughly inspired by Wooga's
%% <a href="https://github.com/wooga/eredis">eredis</a> client.
%%
%% kterl_client manages a single connection to one kyoto tycoon
%% server, using an active socket (tcp data is passed to this
%% gen_server in a message received by the handle_info() callback.)
%%
%% Once a connection is established, callers using the kterl.erl
%% module pass request parameters as iolists to http_request() or
%% bin_request(). These iolists are then written immediately to the
%% kyoto tycoon server socket, and if that's successful, the caller's
%% process id is added to a fifo queue. The caller blocks in
%% gen_server:call() until the gen_server has received the full
%% response to the caller's request.
%%
%% Data received from the socket is passed to kterl_response:parse().
%% Once a full response has been read from the socket, a process id is
%% popped from the fifo queue, and the response is sent to that
%% process id using gen_server:reply().
%%
%% If a connection fails, kterl_client will flush all queued requests
%% and send an error message to any processes waiting for a
%% response. It will then attempt to reestablish a connection to the
%% server. It'll sleep for a fixed number of configurable milliseconds
%% until success.
%%

-module(kterl_client).
-author('rbeer@beerman.org').

-behaviour(gen_server).

-include("kterl.hrl").
-include("ktinternal.hrl").

-export([start_link/3, stop/1, get_cursor/1, local_stats/1, 
         http_request/4, bin_request/2, garbage_collect/1, configure/2]).

%%

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).


-type kt_client_config_param() :: {wire_dump, boolean()}.

-record(state, {
          host                                 :: string(),
          port                                 :: inet:port_number(),
          host_str                             :: binary(),
          reconnect_sleep                      :: non_neg_integer(),
          socket                               :: port(),
          queue = queue:new()                  :: queue(),
          buffer = <<>>                        :: binary(),
          reconnect_pid                        :: pid(),
          last_cursor_id = 0                   :: non_neg_integer(),
          wire_dump = false                    :: boolean(),
          parser_state = kterl_response:init() :: parser_state(),
          n_requests = 0                       :: integer(),
          n_reconnects = 0                     :: integer()
}).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(CONNECT_TIMEOUT, timer:seconds(5)).

-spec start_link(
        Host           :: list(),  
        Port           :: inet:port_number(),
        ReconnectSleep :: non_neg_integer()) -> 
    {ok, Pid::pid()} | {error, Reason::term()}.

start_link(Host, Port, ReconnectSleep) ->
    gen_server:start_link(?MODULE, [Host, Port, ReconnectSleep], []).


-spec stop(Pid::pid()) -> ok.
    
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec local_stats(Pid::pid()) -> [{atom(), any()}].

local_stats(Pid) ->
    gen_server:call(Pid, local_stats).

-spec http_request(
        Pid     :: pid(),
        Reqtype :: binary(),
        Body    :: iolist(),
        Bodylen :: non_neg_integer()) -> #http_response{} | {error, term()}.

http_request(Pid, Reqtype, Body, Bodylen) ->
    gen_server:call(Pid, {http_request, Reqtype, Body, Bodylen}, infinity).

-spec bin_request(
        Pid :: pid(),
        Req :: iolist()) -> #binary_response{} | {error, term()}.

bin_request(Pid, Req) ->
    gen_server:call(Pid, {bin_request, Req}, infinity).

-spec garbage_collect(Pid::pid()) -> ok.

garbage_collect(Pid) ->
    gen_server:call(Pid, garbage_collect).

-spec configure(Pid::pid(), Conf::[{atom(), any()}]) -> 'ok'.

configure(Pid, Conf) ->
    gen_server:call(Pid, {configure, Conf}).

-spec get_cursor(Pid::pid()) -> non_neg_integer().

get_cursor(Pid) ->
    gen_server:call(Pid, get_cursor).

%% =====================================================================
%% gen_server callbacks
%% =====================================================================

init([Host, Port, ReconnectSleep]) ->
    State = initial_state(Host, Port, ReconnectSleep),
    case connect(State) of
        OK = {ok, _} -> 
            OK;
        {error, Why} ->
            {stop, Why}
    end.

handle_call({http_request, Reqtype, Body, Bodylen}, From, State) ->
    do_request(Reqtype, Body, Bodylen, From, State);

handle_call({bin_request, Req}, From, State) ->
    do_bin_request(Req, From, State);

handle_call(stop, _, State) ->
    ok = kill_state(State),
    {stop, normal, ok, initial_state(State)};

handle_call(get_cursor, _From, State = #state{last_cursor_id = LC}) ->
    NC = LC + 1,
    {reply, NC, State#state{last_cursor_id = NC}};

handle_call(local_stats, _From, State) ->
    {reply, get_stats(State), State};

handle_call(garbage_collect, _From, State) ->
    {reply, erlang:garbage_collect(), State};

handle_call({configure, ConfArgs}, _From, State) ->
    {reply, ok, apply_config(ConfArgs, State)}.


handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info({tcp, _Socket, Data}, State) ->
    ok = inet:setopts(State#state.socket, [{active, once}]),
    {noreply, handle_response(Data, State)};

handle_info({tcp_closed, _Socket}, State) ->
    error_logger:error_msg(
      "Connection to Kyoto Tycoon @ ~s closed by foreign host.", 
      [host_str(State)]),
    %% notify any pending callers that the connection is gone..
    flush_queue(State#state.queue, {error, connection_closed}),
    Self = self(),
    RPid = spawn_link(
             fun() -> 
                     reconnect_loop(
                       Self, State#state.host, State#state.port,
                       State#state.reconnect_sleep)
             end),
    {noreply, State#state{socket = undefined, 
                          queue = queue:new(),
                          buffer = <<>>,
                          parser_state = undefined,
                          reconnect_pid = RPid}};

handle_info(stop, State) ->
    ok = kill_state(State),
    {stop, shutdown, initial_state(State)};

handle_info({connection_ready, Socket}, 
            #state{socket = undefined, n_reconnects = NR} = State) ->
    {noreply, State#state{socket = Socket, 
                          reconnect_pid = undefined, 
                          n_reconnects = NR + 1}};

handle_info(Info, State) ->
    error_logger:error_msg(
      "Unexpected message received ~p State = ~p",
      [Info, State]),
    ok = kill_state(State),
    {stop, {unhandled_message, Info}, initial_state(State)}.



terminate(_Reason, State) ->
    ok = kill_state(State).


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% =====================================================================
%% internal functions
%% =====================================================================

do_request(_Reqtype, _Body, _Bodylen, _From, #state{socket = undefined} = State) ->
    {reply, {error, no_connection}, State};

do_request(Reqtype, Body, Bodylen, From, State) ->
    Reqv = kterl_request:gen_request_iolist(State#state.host_str, Reqtype, Body, Bodylen),
    send_queue_req(req, Reqv, From, State).

do_bin_request(Req, From, State) ->
    send_queue_req(bin_req, Req, From, State).

send_queue_req(ReqType, Req, From, State) ->
    wire_dump(State, "sending ~p:~p~n", [ReqType, Req]),
    case gen_tcp:send(State#state.socket, Req) of
        ok ->
            NQ = queue:in({ReqType, From}, State#state.queue),
            NR = State#state.n_requests + 1,
            {noreply, State#state{queue = NQ, n_requests = NR}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.


-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.

%% @doc data was received on the socket. Combine it with any data remaining in
%% the buffer, clear the (now obsolete) buffer, and then process the combined 
%% data.
handle_response(SocketData, #state{buffer = Buffer} = State) ->
    wire_dump(State, "received ~p~n(received bytes=~p buffer bytes=~p)~n", 
              [SocketData, byte_size(SocketData), byte_size(Buffer)]),
    Data = case byte_size(Buffer) of
               0 -> SocketData;
               N when N > 0 -> <<Buffer/binary, SocketData/binary>>
           end,
    process_response(Data, State#state{buffer = <<>>}).

-spec process_response(
        Data   :: binary(),
        State  :: #state{}) -> NewState::#state{}.

process_response(
  Data,
  #state{queue = Queue, parser_state = ParserState} = State) ->
    case kterl_response:parse(ParserState, Data) of
        {continue, NewParserState} ->
            State#state{parser_state = NewParserState};
        {continue, NewParserState, DeferredData} ->
            State#state{parser_state = NewParserState, buffer = DeferredData};
        {Response, NewParserState, DeferredData} ->
            NewQueue = reply(Response, Queue),
            NewState = State#state{parser_state = NewParserState, queue = NewQueue},
            case byte_size(DeferredData) > 0 of
                true ->
                    process_response(DeferredData, NewState);
                false ->
                    NewState
            end
    end.

-spec reply(Value::#http_response{}, Queue::queue()) -> queue()
        ;  (Value::#binary_response{}, Queue::queue())-> queue().

reply(Value, Queue) ->
    case queue:out(Queue) of
        {{value, {_ReqType, From}}, NewQueue} ->
            gen_server:reply(From, Value),
            NewQueue
    end.

-spec connect(State :: #state{}) -> {ok, #state{}} | {error, term()}.

connect(State = #state{host = Host, port = Port}) ->
    case connect(Host, Port) of
        {ok, S} ->
            {ok, State#state{socket = S}};
        Err ->
            Err
    end.

-spec connect(Host :: string(), Port :: inet:port_number()) ->
                     {ok, port()} | {error, term()}.

connect(Host, Port) ->
    error_logger:info_msg("Attempting connection to ~s:~p",[Host,Port]),
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS, ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            error_logger:info_msg("Connected to Kyoto Tycoon server at ~s:~p", 
                                  [Host, Port]),
            {ok, Socket};
        {error, Why} ->
            error_logger:error_msg(
              "Unable to connect to Kyoto Tycoon server at ~s:~p Reason: ~p", 
              [Host,Port,Why]),
            {error, {connection_error, Why}}
    end.

-spec disconnect(#state{}) -> 'ok'.

disconnect(#state{socket = undefined}) -> ok;
disconnect(State = #state{socket = S}) ->
    Res = gen_tcp:close(S),
    error_logger:info_msg(
      "Connection closed to Kyoto Tycoon Server @ ~s = ~p", 
      [host_str(State),Res]).

-spec flush_queue(Queue::queue(), Message::term()) -> ok.

flush_queue(Queue, Message) ->
    case queue:len(Queue) of
        0 -> ok;
        N -> 
            error_logger:info_msg("Flushing ~p queued req(s)", [N]),
            lists:foreach(
              fun({_ReqType, From}) -> gen_server:reply(From, Message) end,
              queue:to_list(Queue))
    end.

-spec reconnect_loop(
        Client :: pid(), 
        Host :: string(), 
        Port :: non_neg_integer(), 
        ReconnectSleep :: non_neg_integer()) -> no_return().
    
reconnect_loop(Client, Host, Port, ReconnectSleep) ->
    case connect(Host, Port) of 
        {ok, Socket} ->
            ok = gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket},
            exit(normal);
        _Err ->
            error_logger:info_msg(
              "Retrying connection to KT server in ~p ms...", 
              [ReconnectSleep]),
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, Host, Port, ReconnectSleep)
    end.

-spec host_str(#state{}) -> binary().

host_str(#state{host_str = Host_str}) -> Host_str.

-spec initial_state(#state{}) -> #state{}.

initial_state(#state{host = Host, 
                     port = Port, 
                     reconnect_sleep = ReconnectSleep}) ->
    initial_state(Host, Port, ReconnectSleep).

-spec initial_state(
        Host           :: string(),
        Port           :: inet:port_number(),
        ReconnectSleep :: non_neg_integer()) -> #state{}.

initial_state(Host, Port, ReconnectSleep) ->
    #state{host = Host, port = Port, 
           host_str = list_to_binary(Host ++ ":" ++ integer_to_list(Port)),
           reconnect_sleep = ReconnectSleep}.

-spec get_stats(#state{}) -> [{atom(), integer()}].

get_stats(#state{n_requests = Nreq, n_reconnects = Nrec}) ->
    [{n_requests, Nreq}, {n_reconnects, Nrec}].

-spec kill_state(#state{}) -> ok.

kill_state(State) ->
    lists:foreach(
      fun(reconnect_pid) ->
              Pid = State#state.reconnect_pid,
              case is_pid(Pid) of
                  true -> erlang:exit(Pid, kill);
                  false -> ok
              end;
         (socket) ->
              disconnect(State);
         (queue) ->
              flush_queue(State#state.queue, {error, shutdown});
         (Stage) ->
              throw({error, {unknown_state, Stage}})
      end, [reconnect_pid, socket, queue]),
    ok.

-spec apply_config(
        ConfArgs :: [kt_client_config_param()],
        State    :: #state{}) -> NewState::#state{}.

apply_config([], State) -> State;
apply_config([{wire_dump, TF} | T], State) when TF =:= true; TF =:= false ->
    apply_config(T, State#state{wire_dump = TF});
apply_config([BadArg | _], _) ->
    throw({error, {invalid_config, BadArg}}).

-spec wire_dump(#state{}, string(), [term()]) -> ok.

wire_dump(#state{wire_dump = WD}, Fmt, Args) when WD =:= true ->
    error_logger:info_msg(Fmt, Args);
wire_dump(_, _, _) ->
    ok.
