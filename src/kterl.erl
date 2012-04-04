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

%% @docfile "doc/edoc/intro.edoc"

-module(kterl).

-author('rbeer@beerman.org').

-include("kterl.hrl").
-include("ktinternal.hrl").
-include("ktbin.hrl").

-export([start_link/0, start_link/1, start_link/3, stop/1]).

-export([void/1, echo/2, report/1, play_script/2, play_script/3,
         play_script/4, tune_replication/2, status/1, status/2,
         clear/1, clear/2, synchronize/1, synchronize/2, set/2, set/3,
         set/4, add/2, add/3, add/4, replace/2, replace/3, replace/4,
         append/3, append/4, increment/3, increment/4,
         increment_double/3, increment_double/4, cas/4, cas/3,
         remove/2, remove/3, get/2, get/3, seize/2, seize/3,
         set_bulk/2, set_bulk/3, remove_bulk/2, remove_bulk/3,
         get_bulk/2, get_bulk/3, vacuum/1, vacuum/2, match_prefix/2,
         match_prefix/3, match_regex/2, match_regex/3]).

-export([cursor/1, release_cursor/1]).

-export([cur_jump/1, cur_jump/2, cur_jump_opt/2, cur_jump_back/1,
         cur_jump_back/2, cur_jump_back_opt/2, cur_step/1, cur_step/2,
         cur_step_back/1, cur_step_back/2, cur_set_value/2,
         cur_set_value/3, cur_set_value_opt/2, cur_remove/1,
         cur_remove/2, cur_get_key/1, cur_get_key/2, cur_get_key/3,
         cur_get_value/1, cur_get_value/2, cur_get_value/3, cur_get/1,
         cur_get/2, cur_get/3, cur_seize/1, cur_seize/2]).

-export([bin_play_script/3, bin_set_bulk/2, bin_remove_bulk/2, bin_get_bulk/2]).

-export([local_stats/1, garbage_collect/1, configure/2]).

-type kt_str()           :: string() | binary().
-type kt_key()           :: kt_str().
-type kt_key_list()      :: [kt_key()].
-type kt_value()         :: kt_str() | number() | atom().
-type kt_kv()            :: {kt_key(), kt_value()}.
-type kt_bin_kv()        :: {Key::binary(), Value::binary()}.
-type kt_kv_list()       :: [kt_kv()].
-type kt_bin_kv_list()   :: [kt_bin_kv()].
-type kt_call_kvl()      :: kt_kv_list() | dict().
-type kt_exptime()       :: non_neg_integer() | calendar:datetime().
-type kt_client()        :: pid().
-type kt_cursor()        :: #kterl_cursor{cursor_id :: non_neg_integer(), 
                                          client_pid :: pid()}.
-type kt_database()      :: non_neg_integer() | string() | binary().
-type kt_optargs()       :: [{atom(), atom() | number() | string() | binary()
                          |   kt_exptime()}].
-type kt_bin_rec()       :: #kt_bin_rec{dbidx :: non_neg_integer(),
                                        xt :: non_neg_integer(),
                                        key :: binary(),
                                        val :: binary()}.
-type kt_http_status()   :: 200 | 400 | 450 | 500 | 501 | 503.
-type kt_http_error()    :: {error, {http_result, StatusCode::kt_http_status(), 
                                     Err::binary() | term()}}.

-type kt_http_result()   :: #kt_http_result{key :: binary(),
                                            value :: binary(),
                                            exptime :: non_neg_integer(),
                                            signaled_count :: non_neg_integer(),
                                            num :: number(),
                                            keys :: [binary()],
                                            bulk_records :: 
                                              [{Key :: binary(), 
                                                Value :: binary()}]}.
                                                            
-export_type([kt_str/0, kt_key/0, kt_key_list/0, kt_value/0, 
              kt_kv/0, kt_kv_list/0, kt_bin_kv/0, kt_bin_kv_list/0, 
              kt_call_kvl/0, kt_exptime/0, kt_client/0, kt_cursor/0, 
              kt_database/0, kt_optargs/0, kt_bin_rec/0, kt_http_result/0]).

%% @private
-type result_proc()    :: [{kt_http_status(), function() | term() | atom()}].

%% @doc Connects to database 0 @ localhost:1978 with a 5 second connection 
%% retry interval. Creates the connection handler and links to the calling process.
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link("127.0.0.1",1978,5000).

%% @doc Connects to 'host' on port number 'port'
%% with a connection retry interval of 'reconnect_sleep' ms.
%% Creates the connection handler and links to calling process.
-spec start_link(Args :: [{host, string()}
                       |  {port, non_neg_integer()}
                       |  {reconnect_sleep, non_neg_integer()}]) -> 
    {ok,pid()} | {error, term()}.
    
start_link(Args) ->
    Host = proplists:get_value(host, Args, "127.0.0.1"),
    Port = proplists:get_value(port, Args, 1978),
    ReconnectSleep = proplists:get_value(reconnect_sleep, Args, 100),
    start_link(Host, Port, ReconnectSleep).


%% @doc Connects to Host : Port with a connection retry interval of 
%% ReconnectSleep ms. Creates the connection handler and links to calling 
%% process.
-spec start_link(
        Host           :: string(), 
        Port           :: inet:port_number(),
        ReconnectSleep :: non_neg_integer()) ->
    {ok, pid()} | {error, term()}.

start_link(Host, Port, ReconnectSleep) 
  when is_list(Host), is_integer(Port), is_integer(ReconnectSleep) ->
    kterl_client:start_link(Host, Port, ReconnectSleep).


%% @doc Gracefully closes the tcp connection and stops the gen_server 
%% connection handler.
-spec stop(Client::pid()) -> ok.

stop(Client) ->
    kterl_client:stop(client_pid(Client)).

%% @doc Returns stats maintained by the local connection handler.
-spec local_stats(Client::kt_client()) -> [{atom(), any()}].

local_stats(Client) ->
    kterl_client:local_stats(client_pid(Client)).

%% @doc Forces the connection handler to perform garbage collection.
-spec garbage_collect(Client::kt_client()) -> 'ok'.

garbage_collect(Client) ->
    kterl_client:garbage_collect(client_pid(Client)).

%% @doc Configures the connection handler.<br/>
%% <code>{wire_dump, boolean()}</code> to send all wire traffic to error_logger 
%% (default: false)
-spec configure(
        Client :: kt_client(), 
        Conf   :: [{wire_dump, boolean()}]) -> 'ok'.

configure(Client, Conf) ->
    kterl_client:configure(client_pid(Client), Conf).

%% =====================================================================
%% http calls
%% =====================================================================

%% __ret__ void/1 ok
%% @docfile "doc/edoc/void_1.edoc"
-spec void(Client::kt_client()) -> 'ok' | kt_http_error().

void(Client) ->
    call(Client, <<"void">>, [{200, ok}]).

%% __ret__ echo/2 get_records get_num
%% @docfile "doc/edoc/echo_2.edoc"
-spec echo(
        Client :: kt_client(), 
        KVL    :: kt_call_kvl()) -> 
    {ok, kt_http_result()} | kt_http_error().
    
echo(Client, KVL) ->
    call(Client, <<"echo">>, [{200, rt_kvl()}], [arg_kvl(KVL)]).

%% __ret__ report/1 get_records get_num
%% @docfile "doc/edoc/report_1.edoc"
-spec report(Client::kt_client()) -> 
    {ok, kt_http_result()} | kt_http_error().

report(Client) ->
    call(Client, <<"report">>, [{200, rt_kvl()}]).


%% __ret__ play_script/2 get_records get_num
%% @docfile "doc/edoc/play_script_2.edoc"
-spec play_script(Client :: kt_client(),
                  Name   :: kt_str()) -> 
                         {ok, kt_http_result()} | kt_http_error().
play_script(Client, Name) -> 
    call(Client, 
         <<"play_script">>,
         [{200, rt_ukvl()},
          {450, rt_error()}],
         [arg_kv(<<"name">>, Name)]).

%% __ret__ play_script/3 get_records get_num
%% @docfile "doc/edoc/play_script_3.edoc"
-spec play_script(Client     :: kt_client(),
                  Name       :: kt_str(),
                  ScriptArgs :: kt_call_kvl()) -> 
    {ok, kt_http_result()} | kt_http_error().

play_script(Client, Name, ScriptArgs) ->
    call(Client, 
         <<"play_script">>,
         [{200, rt_ukvl()}, 
          {450, rt_error()}],
         [arg_kv(<<"name">>, Name), 
          arg_ukvl(ScriptArgs)]).

%% __ret__ play_script/4 get_records get_num get_signaled_count
%% @docfile "doc/edoc/play_script_4.edoc"
-spec play_script(Client     :: kt_client(),
                  Name       :: kt_str(),
                  ScriptArgs :: kt_call_kvl(),
                  OptArgs :: [{wait,     binary() | string}
                           |  {waittime, binary() | string() | 
                               non_neg_integer()}
                           |  {signal,   binary() | string()}
                           |  {signalbroad, boolean()}]) 
                 -> {ok, kt_http_result()} | kt_http_error() 
                  | {error, timed_out}.

play_script(Client, Name, ScriptArgs, OptArgs) ->
    call(Client,
         <<"play_script">>,
         [{200, rt_ukvl()},
          {450, rt_error()},
          {503, {error, timed_out}}],
         [arg_kv(<<"name">>, Name),
          arg_ukvl(ScriptArgs),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])]).

%% __ret__ tune_replication/2 get_signaled_count
%% @docfile "doc/edoc/tune_replication_2.edoc"
-spec tune_replication(
        Client  :: kt_client(), 
        OptArgs :: [{host,        binary() | string()} 
                 |  {port,        non_neg_integer()}
                 |  {ts,          non_neg_integer()}
                 |  {iv,          non_neg_integer()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
                      -> {ok, kt_http_result()} 
                       | {error, timed_out}  
                       | kt_http_error().

tune_replication(Client, OptArgs) ->
    call(Client, 
         <<"tune_replication">>,
         [{200, rt_signal()},
          {503, {error, timed_out}}],
         [arg_multi_opt(
            OptArgs, 
            [{host,     {kv,  <<"host">>}}, 
             {port,     {kv,  <<"port">>}}, 
             {ts,       {kv,  <<"ts">>}}, 
             {iv,       {kv,  <<"iv">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ status/1 get_records
%% @docfile "doc/edoc/status_1.edoc"
-spec status(Client::kt_client()) -> {ok, kt_http_result()} | kt_http_error().

status(Client) ->
    call(Client, <<"status">>, [{200, rt_kvl()}]).

%% __ret__ status/2 get_records
%% @docfile "doc/edoc/status_2.edoc"
-spec status(
         Client   :: kt_client(), 
         Database :: kt_database()) -> {ok, kt_http_result()} | kt_http_error().

status(Client, Database) ->
    call(Client, <<"status">>, [{200, rt_kvl()}], [arg_kv(<<"DB">>, Database)]).

%% __ret__ clear/1 ok
%% @docfile "doc/edoc/clear_1.edoc"
-spec clear(Client::kt_client()) -> ok | kt_http_error().
    
clear(Client) ->
    call(Client, <<"clear">>, [{200, ok}]).

%% __ret__ clear/2 get_signaled_count
%% @docfile "doc/edoc/clear_2.edoc"
-spec clear(
        Client   :: kt_client(), 
        OptArgs  :: [{database,    kt_database()}
                  |  {wait,        binary() | string()}
                  |  {waittime,    binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                 | {error, timed_out}  
                                                 | kt_http_error().

clear(Client, OptArgs) ->
    call(Client, <<"clear">>, 
         [{200, rt_signal()},
          {503, {error, timed_out}}], 
         [arg_multi_opt(
            OptArgs,
            [{database,     {kv,   <<"DB">>}},
             {wait,         {kv,   <<"WAIT">>}},
             {waittime,     {kv,   <<"WAITTIME">>}},
             {signal,       {kv,   <<"SIGNAL">>}},
             {signalbroad,  {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ synchronize/1 ok
%% @docfile "doc/edoc/synchronize_1.edoc"
-spec synchronize(Client::kt_client()) -> ok | kt_http_error().

synchronize(Client) ->
    call(Client, <<"synchronize">>, [{200, ok}, {450, rt_error()}]).

%% __ret__ synchronize/2 get_signaled_count
%% @docfile "doc/edoc/synchronize_2.edoc"
-spec synchronize(
        Client  :: kt_client(), 
        OptArgs :: [{database,    kt_database()}
                 |  {hard,        boolean()}
                 |  {command,     binary() | string()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, timed_out} 
                                                | kt_http_error().

synchronize(Client, OptArgs) ->
    call(Client, <<"synchronize">>, 
         [{200, rt_signal()}, 
          {450, rt_error()},
          {503, {error, timed_out}}],
         [arg_multi_opt(
            OptArgs,
            [{database, {kv,   <<"DB">>}},
             {hard,     {flag, <<"hard">>}},
             {command,  {kv,   <<"command">>}},
             {wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ set/2 ok
%% @docfile "doc/edoc/set_2.edoc"
-spec set(Client   :: kt_client(), 
          KeyValue :: {Key::kt_key(), Value::kt_value()}) -> 
                 ok | kt_http_error().

set(Client, {Key,Value}) ->
    set(Client, Key, Value).

%% __ret__ set/3 ok
%% @docfile "doc/edoc/set_3.edoc"
-spec set(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Value  :: kt_value()) -> ok | kt_http_error().
    
set(Client, Key, Value) ->
    call(Client, <<"set">>, 
         [{200, ok}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value)]).

%% __ret__ set/4 get_signaled_count
%% @docfile "doc/edoc/set_4.edoc"
-spec set(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Value   :: kt_value(), 
        OptArgs :: [{database,    kt_database()} 
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                | {error, timed_out} 
                                                | kt_http_error().

set(Client, Key, Value, OptArgs) ->
    call(Client, <<"set">>, 
         [{200, rt_signal()},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {xt,       {xt,  <<"xt">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ add/2 ok
%% @docfile "doc/edoc/add_2.edoc"
-spec add(
        Client   :: kt_client(), 
        KeyValue :: {Key::kt_key(), Value::kt_value()}) 
         -> ok | {error, duplicate_key} | kt_http_error().

add(Client, {Key, Value}) ->
    add(Client, Key, Value).

%% __ret__ add/3 ok
%% @docfile "doc/edoc/add_3.edoc"
-spec add(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Value  :: kt_value()) -> ok
                               | {error, duplicate_key} 
                               | kt_http_error().

add(Client, Key, Value) ->
    call(Client, <<"add">>,
         [{200, ok},
          {450, {error, duplicate_key}}], 
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value)]).

%% __ret__ add/4 get_signaled_count
%% @docfile "doc/edoc/add_4.edoc"
-spec add(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Value   :: kt_value(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
         -> {ok, kt_http_result()} | {error, duplicate_key} | {error, timed_out}
                | kt_http_error().

add(Client, Key, Value, OptArgs) ->
    call(Client, <<"add">>,
         [{200, rt_signal()},
          {450, {error, duplicate_key}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {xt,       {xt,  <<"xt">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ replace/2 ok
%% @docfile "doc/edoc/replace_2.edoc"
-spec replace(
        Client   :: kt_client(), 
        KeyValue :: kt_kv()) -> ok
                              | {error, no_record} 
                              | kt_http_error().

replace(Client, {Key, Value}) ->
    replace(Client, Key, Value).

%% __ret__ replace/3 ok
%% @docfile "doc/edoc/replace_3.edoc"
-spec replace(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Value  :: kt_value()) -> ok
                               | {error, no_record} 
                               | kt_http_error().

replace(Client, Key, Value) ->
    call(Client, <<"replace">>, 
         [{200, ok},
          {450, {error, no_record}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value)]).

%% __ret__ replace/4 get_signaled_count
%% @docfile "doc/edoc/replace_4.edoc"
-spec replace(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Value   :: kt_value(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                | {error, no_record} 
                                                | {error, timed_out}
                                                | kt_http_error().

replace(Client, Key, Value, OptArgs) ->
    call(Client, <<"replace">>, 
         [{200, rt_signal()}, 
          {450, {error, no_record}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {xt,       {xt,  <<"xt">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ append/3 ok
%% @docfile "doc/edoc/append_3.edoc"
-spec append(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Value  :: kt_value()) -> ok | kt_http_error().

append(Client, Key, Value) ->
    call(Client, <<"append">>, 
         [{200, ok}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value)]).

%% __ret__ append/4 get_signaled_count
%% @docfile "doc/edoc/append_4.edoc"
-spec append(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Value   :: kt_value(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                | {error, timed_out} 
                                                | kt_http_error().

append(Client, Key, Value, OptArgs) ->
    call(Client, <<"append">>, 
         [{200, rt_signal()},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"value">>, Value),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {xt,       {xt,  <<"xt">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).


-define(INC_NUM_RT, [{200, rt_num()}, 
                     {450, {error, incompatible_record}}, 
                     {503, {error, timed_out}}]).
-define(INC_DBL_RT, [{200, rt_float()}, 
                     {450, {error, incompatible_record}}, 
                     {503, {error, timed_out}}]).

%% __ret__ increment/3 get_num
%% @docfile "doc/edoc/increment_3.edoc"
-spec increment(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Num    :: string() | binary() | integer()) -> 
    {ok, kt_http_result()} | {error, incompatible_record} | kt_http_error().

increment(Client, Key, Num) ->
    increment_(<<"increment">>, ?INC_NUM_RT, Client, Key, Num).

%% __ret__ increment/4 get_num get_signaled_count
%% @docfile "doc/edoc/increment_4.edoc"
-spec increment(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Num     :: string() | binary() | integer(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {orig,        integer()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> 
    {ok, kt_http_result()} | {error, incompatible_record} | kt_http_error().

increment(Client, Key, Num, OptArgs) ->
    increment_(<<"increment">>, ?INC_NUM_RT, Client, Key, Num, OptArgs).

%% __ret__ increment_double/3 get_num
%% @docfile "doc/edoc/increment_double_3.edoc"
-spec increment_double(
        Client :: kt_client(), 
        Key    :: kt_key(), 
        Num    :: string() | binary() | number()) -> 
                              {ok, kt_http_result()}
                            | {error, incompatible_record} 
                            | kt_http_error().

increment_double(Client, Key, Num) ->
    increment_(<<"increment_double">>, ?INC_DBL_RT, Client, Key, Num).

%% __ret__ increment_double/4 get_num get_signaled_count
%% @docfile "doc/edoc/increment_double_4.edoc"
-spec increment_double(
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Num     :: string() | binary() | number(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {orig,        number()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
                      -> {ok, kt_http_result()}
                       | {error, incompatible_record} 
                       | kt_http_error().

increment_double(Client, Key, Num, OptArgs) ->
    increment_(<<"increment_double">>, ?INC_DBL_RT, Client, Key, Num, OptArgs).

-undef(INC_NUM_RT).
-undef(INC_DBL_RT).

-spec increment_(
        Method   :: binary(), 
        ResProc  :: result_proc(),
        Client   :: kt_client(), 
        Key      :: kt_key(), 
        Num      :: string() | binary() | number()) 
                -> {ok, kt_http_result()}
                 | {error, incompatible_record} 
                 | kt_http_error().

increment_(Method, ResProc, Client, Key, Num) ->
    call(Client, Method, ResProc,
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"num">>, Num)]).

-spec increment_(
        Method  :: binary(),
        ResProc :: result_proc(),
        Client  :: kt_client(), 
        Key     :: kt_key(), 
        Num     :: string() | binary() | number(), 
        OptArgs :: [{database,    kt_database()}
                 |  {xt,          kt_exptime()}
                 |  {orig,        number()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
                -> {ok, kt_http_result()}
                 | {error, incompatible_record} 
                 | kt_http_error().

increment_(Method, ResProc, Client, Key, Num, OptArgs) ->
    call(Client, Method, ResProc,
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"num">>, Num),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {xt,       {xt,  <<"xt">>}},
             {orig,     {kv,  <<"orig">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ cas/4 ok
%% @docfile "doc/edoc/cas_4.edoc"
-spec cas(
        Client   :: kt_client(),
        Key      :: kt_key(),
        OldValue :: kt_value(),
        NewValue :: kt_value()) 
         -> ok
          | {error, expired_value}
          | kt_http_error().

cas(Client, Key, OldValue, NewValue) ->
    call(Client, <<"cas">>,
         [{200, ok},
          {450, {error, expired_value}}],
         [arg_kv(<<"key">>, Key),
          arg_kv(<<"oval">>, OldValue),
          arg_kv(<<"nval">>, NewValue)]).

%% __ret__ cas/3 get_signaled_count
%% @docfile "doc/edoc/cas_3.edoc"
-spec cas(
        Client   :: kt_client(), 
        Key      :: kt_key(), 
        OptArgs :: [{database,    kt_database()}
                 |  {oval,        kt_value()}
                 |  {nval,        kt_value()}
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
         -> {ok, kt_http_result()}
         | {error, expired_value} 
         | {error, timed_out}
         | kt_http_error().

cas(Client, Key, OptArgs) ->
    call(Client, <<"cas">>, 
         [{200, rt_signal()}, 
          {450, {error, expired_value}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {oval,     {kv,  <<"oval">>}},
             {nval,     {kv,  <<"nval">>}},
             {xt,       {xt,  <<"xt">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ remove/2 ok
%% @docfile "doc/edoc/remove_2.edoc"
-spec remove(
        Client :: kt_client(),
        Key    :: kt_key()) -> ok
                             | {error, no_record} 
                             | kt_http_error().

remove(Client, Key) ->
    call(Client, <<"remove">>, [{200, ok}, {450, {error, no_record}}],
         [arg_kv(<<"key">>, Key)]).

%% __ret__ remove/3 get_signaled_count
%% @docfile "doc/edoc/remove_3.edoc"
-spec remove(
        Client   :: kt_client(),
        Key      :: kt_key(),
        OptArgs  :: [{database,    kt_database()}
                  |  {wait,        binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                 | {error, no_record} 
                                                 | {error, timed_out}
                                                 | kt_http_error().

remove(Client, Key, OptArgs) ->
    call(Client, <<"remove">>, 
         [{200, rt_signal()}, 
          {450, {error, no_record}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_multi_opt(
            OptArgs,
            [{database,    {kv,  <<"DB">>}},
             {wait,        {kv,  <<"WAITTIME">>}},
             {signal,      {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag,  <<"SIGNALBROAD">>}}])
          ]).


%% __ret__ get/2 get_value get_exptime
%% @docfile "doc/edoc/get_2.edoc"
-spec get(
        Client :: kt_client(),
        Key    :: kt_key()) -> {ok, kt_http_result()}
                             | {error, no_record} 
                             | kt_http_error().

get(Client, Key) ->
    call(Client, <<"get">>, 
         [{200, rt_value()}, 
          {450, {error, no_record}}],
         [arg_kv(<<"key">>, Key)]).

%% __ret__ get/3 get_value get_exptime get_signaled_count
%% @docfile "doc/edoc/get_3.edoc"
-spec get(
        Client   :: kt_client(),
        Key      :: kt_key(),
        OptArgs  :: [{database, kt_database()}
                  |  {wait,     binary() | string()}
                  |  {waittime, non_neg_integer()}
                  |  {signal,   binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                 | {error, no_record} 
                                                 | {error, timed_out}
                                                 | kt_http_error().

get(Client, Key, OptArgs) ->
    call(Client, <<"get">>, 
         [{200, rt_value()}, 
          {450, {error, no_record}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_multi_opt(
            OptArgs,
            [{database,      {kv, <<"DB">>}},
             {wait,          {kv, <<"WAIT">>}},
             {waittime,      {kv, <<"WAITTIME">>}},
             {signal,        {kv, <<"SIGNAL">>}},
             {signalbroad,   {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ seize/2 get_value get_exptime
%% @docfile "doc/edoc/seize_2.edoc"
-spec seize(
        Client :: kt_client(),
        Key    :: kt_key()) ->
                   {ok, kt_http_result()} | {error, no_record} | kt_http_error().

seize(Client, Key) ->
    call(Client, <<"seize">>, 
         [{200, rt_value()}, 
          {450, {error, no_record}}],
         [arg_kv(<<"key">>, Key)]).

%% __ret__ seize/3 get_value get_exptime get_signaled_count
%% @docfile "doc/edoc/seize_3.edoc"
-spec seize(
        Client   :: kt_client(),
        Key      :: kt_key(),
        OptArgs  :: [{database,   kt_database()}
                  |  {wait, binary() | string()}
                  |  {waittime, binary() | string() | non_neg_integer()}
                  |  {signal, binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                 | {error, no_record} 
                                                 | {error, timed_out}
                                                 | kt_http_error().

seize(Client, Key, OptArgs) ->
    call(Client, <<"seize">>, 
         [{200, rt_value()}, 
          {450, {error, no_record}},
          {503, {error, timed_out}}],
         [arg_kv(<<"key">>, Key),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ set_bulk/2 get_num
%% @docfile "doc/edoc/set_bulk_2.edoc"
-spec set_bulk(
        Client   :: kt_client(),
        KVL      :: dict() | kt_kv_list()) -> 
                      {ok, kt_http_result()} | kt_http_error().

set_bulk(Client, KVL) ->                   
    call(Client, <<"set_bulk">>, [{200, rt_num()}], [arg_ukvl(KVL)]).

%% __ret__ set_bulk/3 get_num get_signaled_count
%% @docfile "doc/edoc/set_bulk_3.edoc"
-spec set_bulk(
        Client   :: kt_client(),
        KVL      :: dict() | kt_kv_list(),
        OptArgs  :: [{database,    kt_database()}
                  |  {atomic,      boolean()}
                  |  {xt,          kt_exptime()}
                  |  {wait,        binary() | string()}
                  |  {waittime,    binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) 
              -> {ok, kt_http_result()} | {error, timed_out} | kt_http_error().

set_bulk(Client, KVL, OptArgs) ->
    call(Client, <<"set_bulk">>, 
         [{200, rt_num()},
          {503, {error, timed_out}}],
         [arg_ukvl(KVL),
          arg_multi_opt(
            OptArgs,
            [{xt,       {xt,   <<"xt">>}},
             {atomic,   {flag, <<"atomic">>}},
             {database, {kv,   <<"DB">>}},
             {wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ remove_bulk/2 get_num
%% @docfile "doc/edoc/remove_bulk_2.edoc"
-spec remove_bulk(
        Client :: kt_client(),
        Keys   :: kt_key_list()) -> {ok, kt_http_result()} | kt_http_error().

remove_bulk(Client, Keys) ->
    call(Client, <<"remove_bulk">>, [{200, rt_num()}], [arg_ukeys(Keys)]).


%% __ret__ remove_bulk/3 get_num get_signaled_count
%% @docfile "doc/edoc/remove_bulk_3.edoc"
-spec remove_bulk(
        Client  :: kt_client(),
        Keys    :: kt_key_list(),
        OptArgs :: [{database,    kt_database()}
                 |  {atomic,      boolean()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
                 -> {ok, kt_http_result()} | {error, timed_out} 
                  | kt_http_error().

remove_bulk(Client, Keys, OptArgs) ->
    call(Client, <<"remove_bulk">>, 
         [{200, rt_num()},
          {503, {error, timed_out}}],
         [arg_ukeys(Keys),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,   <<"DB">>}},
             {atomic,   {flag, <<"atomic">>}},
             {wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ get_bulk/2 get_records
%% @docfile "doc/edoc/get_bulk_2.edoc"
-spec get_bulk(
        Client :: kt_client(),
        Keys   :: kt_key_list()) -> 
                      {ok, kt_http_result()} | kt_http_error().

get_bulk(Client, Keys) ->
    call(Client, <<"get_bulk">>, [{200, rt_ukvl()}], [arg_ukeys(Keys)]).

%% __ret__ get_bulk/3 get_records get_signaled_count
%% @docfile "doc/edoc/get_bulk_3.edoc"
-spec get_bulk(
        Client  :: kt_client(),
        Keys    :: kt_key_list(),
        OptArgs :: [{database,    kt_database()}
                 |  {atomic,      boolean()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) 
              -> {ok, kt_http_result()}
               | {error, timed_out}
               | kt_http_error().

get_bulk(Client, Keys, OptArgs) ->
    call(Client, <<"get_bulk">>, 
         [{200, rt_ukvl()},
          {503, {error, timed_out}}],
         [arg_ukeys(Keys),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,   <<"DB">>}},
             {atomic,   {flag, <<"atomic">>}},
             {wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ vacuum/1 ok
%% @docfile "doc/edoc/vacuum_1.edoc"
-spec vacuum(Client::kt_client()) -> ok | kt_http_error().

vacuum(Client) ->
    call(Client, <<"vacuum">>, [{200, ok}]).

%% __ret__ vacuum/2 get_signaled_count
%% @docfile "doc/edoc/vacuum_2.edoc"
-spec vacuum(
        Client  :: kt_client(), 
        OptArgs :: [{database,    kt_database()}
                 |  {step,        non_neg_integer()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                | {error, timed_out} 
                                                | kt_http_error().

vacuum(Client, OptArgs) ->
    call(Client, <<"vacuum">>, 
         [{200, rt_signal()},
          {503, {error, timed_out}}],
         [arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {step,     {kv,  <<"step">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).


%% __ret__ match_prefix/2 get_keys
%% @docfile "doc/edoc/match_prefix_2.edoc"
-spec match_prefix(Client::kt_client(), Prefix::kt_str()) -> 
                          {ok, kt_http_result()} | kt_http_error().

match_prefix(Client, Prefix) ->
    call(Client, <<"match_prefix">>, [{200, rt_ukey()}],
         [arg_kv(<<"prefix">>, Prefix)]).

%% __ret__ match_prefix/3 get_keys get_signaled_count
%% @docfile "doc/edoc/match_prefix_3.edoc"
-spec match_prefix(
        Client   :: kt_client(),
        Prefix   :: kt_str(),
        OptArgs  :: [{database,    kt_database()}
                  |  {max,         non_neg_integer()}
                  |  {wait,        binary() | string()}
                  |  {waittime,    binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) 
                  -> {ok, kt_http_result()} 
                   | {error, timed_out} 
                   | kt_http_error().

match_prefix(Client, Prefix, OptArgs) ->
    call(Client, <<"match_prefix">>, 
         [{200, rt_ukey()},
          {503, {error, timed_out}}], 
         [arg_kv(<<"prefix">>, Prefix),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {max,      {kv,  <<"max">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ match_regex/2 get_keys
%% @docfile "doc/edoc/match_regex_2.edoc"
-spec match_regex(
        Client::kt_client(), 
        Regex::kt_str()) -> {ok, kt_http_result()} | kt_http_error().

match_regex(Client, Regex) ->
    call(Client, <<"match_regex">>, [{200, rt_ukey()}],
         [arg_kv(<<"regex">>, Regex)]).

%% __ret__ match_regex/3 get_keys get_signaled_count
% @docfile "doc/edoc/match_regex_3.edoc"
-spec match_regex(
        Client   :: kt_client(),
        Regex    :: kt_str(),
        OptArgs  :: [{database,    kt_database()}
                  |  {max,         non_neg_integer()}
                  |  {wait,        binary() | string()}
                  |  {waittime,    binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) 
                 -> {ok, kt_http_result()}
                  | {error, timed_out} 
                  | kt_http_error().

match_regex(Client, Regex, OptArgs) ->
    call(Client, <<"match_regex">>, 
         [{200, rt_ukey()},
          {503, {error, timed_out}}],
         [arg_kv(<<"regex">>, Regex),
          arg_multi_opt(
            OptArgs,
            [{database, {kv,  <<"DB">>}},
             {max,      {kv,  <<"max">>}},
             {wait,     {kv,  <<"WAIT">>}},
             {waittime, {kv,  <<"WAITTIME">>}},
             {signal,   {kv,  <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% @docfile "doc/edoc/cursor_1.edoc"
-spec cursor(Client::kt_client()) -> kt_cursor().

cursor(Client) ->
    Cursor_id = kterl_client:get_cursor(Client),
    #kterl_cursor{cursor_id = Cursor_id, client_pid = client_pid(Client)}.

%% @docfile "doc/edoc/release_cursor_1.edoc"
-spec release_cursor(Cursor :: kt_cursor()) -> ok
                                             | {error, invalid_cursor} 
                                             | kt_http_error().

release_cursor(Cursor) ->
    call(Cursor, <<"cur_delete">>, 
         [{200, ok},
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% cursor operations ======================================================

%% __ret__ cur_jump/1 ok
%% @docfile "doc/edoc/cur_jump_1.edoc"
-spec cur_jump(Cursor :: kt_cursor()) -> ok 
                                       | {error, invalid_cursor}
                                       | {error, not_implemented}
                                       | kt_http_error().

cur_jump(Cursor) ->
    cur_op_jump(Cursor, <<"cur_jump">>, []).

%% __ret__ cur_jump/2 ok
%% @docfile "doc/edoc/cur_jump_2.edoc"
-spec cur_jump(Cursor      :: kt_cursor(),
               KeyDatabase :: [{key, kt_key()} | {database, kt_database()}])
              -> ok | {error, invalid_cursor | not_implemented}
                    | kt_http_error().

cur_jump(Cursor, KeyDatabase = [T|_]) when is_tuple(T) ->
    cur_op_jump(Cursor, <<"cur_jump">>, KeyDatabase).

%% __ret__ cur_jump_opt/2 get_signaled_count
%% @docfile "doc/edoc/cur_jump_opt_2.edoc"
-spec cur_jump_opt(
        Cursor   :: kt_cursor(), 
        OptArgs  :: [{key,        kt_key()}
                  |  {database,   kt_database()}
                  |  {wait,       binary() | string()}
                  |  {waittime,   binary() | string() | non_neg_integer()}
                  |  {signal,     binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                 | {error, not_implemented}
                                                 | {error, invalid_cursor}
                                                 | {error, timed_out}
                                                 | kt_http_error().

cur_jump_opt(Cursor, OptArgs = [T|_]) when is_tuple(T) ->
    cur_sigop_jump(Cursor, <<"cur_jump">>, OptArgs).


%% __ret__ cur_jump_back/1 ok
%% @docfile "doc/edoc/cur_jump_back_1.edoc"
-spec cur_jump_back(Cursor :: kt_cursor()) -> ok
                                            | {error, invalid_cursor}
                                            | {error, not_implemented}
                                            | kt_http_error().

cur_jump_back(Cursor) ->
    cur_op_jump(Cursor, <<"cur_jump_back">>, []).

%% __ret__ cur_jump_back/2 ok
%% @docfile "doc/edoc/cur_jump_back_2.edoc"
-spec cur_jump_back(Cursor      :: kt_cursor(),
                    KeyDatabase :: [{key, kt_key()}
                                 |  {database, kt_database()}]) ->
                         ok 
                         | {error, invalid_cursor} 
                         | {error, not_implemented}
                         | kt_http_error().

cur_jump_back(Cursor, KeyDatabase = [T|_]) when is_tuple(T) ->
    cur_op_jump(Cursor, <<"cur_jump_back">>, KeyDatabase).

%% __ret__ cur_jump_back_opt/2 get_signaled_count
%% @docfile "doc/edoc/cur_jump_back_opt_2.edoc"
-spec cur_jump_back_opt(
        Cursor   :: kt_cursor(), 
        OptArgs  :: [{key,        kt_key()}
                  |  {database,   kt_database()}
                  |  {wait,       binary() | string()}
                  |  {waittime,   binary() | string() | non_neg_integer()}
                  |  {signal,     binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()} 
                                                 | {error, invalid_cursor}
                                                 | {error, not_implemented}
                                                 | {error, timed_out}
                                                 | kt_http_error().

cur_jump_back_opt(Cursor, OptArgs = [T|_]) when is_tuple(T) ->
    cur_sigop_jump(Cursor, <<"cur_jump_back">>, OptArgs).


-spec cur_op_jump(
        Cursor   :: kt_cursor(),
        Op       :: binary(),
        OptArgs  :: [{key,      kt_key()}
                  |  {database, kt_database()}]) -> ok
                                                  | {error, invalid_cursor}
                                                  | {error, not_implemented}
                                                  | kt_http_error().

cur_op_jump(Cursor, Op, OptArgs) ->
    call(Cursor, Op, 
         [{200, ok},
          {450, {error, invalid_cursor}},
          {501, {error, not_implemented}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{key,      {kv, <<"key">>}},
             {database, {kv, <<"DB">>}}])
         ]).

-spec cur_sigop_jump(
        Cursor   :: kt_cursor(), 
        Op       :: binary(), 
        OptArgs  :: [{key,         kt_key()}
                  |  {database,    kt_database()}
                  |  {wait,        binary() | string()}
                  |  {waittime,    binary() | string() | non_neg_integer()}
                  |  {signal,      binary() | string()}
                  |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                 | {error, invalid_cursor}
                                                 | {error, not_implemented}
                                                 | {error, timed_out}
                                                 | kt_http_error().
    
cur_sigop_jump(Cursor, Op, OptArgs) ->
    call(Cursor, Op,
         [{200, rt_signal()}, 
          {450, {error, invalid_cursor}}, 
          {501, {error, not_implemented}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{key,         {kv,   <<"key">>}},
             {database,    {kv,   <<"DB">>}},
             {wait,        {kv,   <<"WAIT">>}},
             {waittime,    {kv,   <<"WAITTIME">>}},
             {signal,      {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ cur_step/1 ok
%% @docfile "doc/edoc/cur_step_1.edoc"
-spec cur_step(Cursor :: kt_cursor()) -> ok
                                       | {error, invalid_cursor}
                                       | kt_http_error().

cur_step(Cursor) ->
    call(Cursor, <<"cur_step">>, 
         [{200, ok},
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_step/2 get_signaled_count
%% @docfile "doc/edoc/cur_step_2.edoc"
-spec cur_step(Cursor  :: kt_cursor(),
               OptArgs :: [{wait,        binary() | string()}
                        |  {waittime,    binary() | string() | non_neg_integer()}
                        |  {signal,      binary() | string()}
                        |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                       | {error, invalid_cursor}
                                                       | {error, timed_out}
                                                       | kt_http_error().

cur_step(Cursor, OptArgs) ->
    call(Cursor, <<"cur_step">>,
         [{200, rt_signal()},
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ cur_step_back/1 ok
%% @docfile "doc/edoc/cur_step_back_1.edoc"
-spec cur_step_back(Cursor :: kt_cursor()) -> ok
                                            | {error, invalid_cursor}
                                            | {error, not_implemented}
                                            | kt_http_error().

cur_step_back(Cursor) ->
    call(Cursor, <<"cur_step_back">>, 
         [{200, ok},
          {450, {error, invalid_cursor}},
          {501, {error, not_implemented}}],
         [arg_cursor(Cursor)]).


%% __ret__ cur_step_back/2 get_signaled_count
%% @docfile "doc/edoc/cur_step_back_2.edoc"
-spec cur_step_back(
        Cursor :: kt_cursor(),
        OptArgs :: [{wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor}
                                                | {error, not_implemented}
                                                | {error, timed_out}
                                                | kt_http_error().

cur_step_back(Cursor, OptArgs) ->
    call(Cursor, <<"cur_step_back">>,
         [{200, rt_signal()},
          {450, {error, invalid_cursor}},
          {501, {error, not_implemented}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).


%% __ret__ cur_set_value/2 ok
%% @docfile "doc/edoc/cur_set_value_2.edoc"
-spec cur_set_value(
        Cursor :: kt_cursor(), 
        Value  :: kt_value()) -> ok
                               | {error, invalid_cursor} 
                               | kt_http_error().
    
cur_set_value(Cursor, Value) ->
    cur_op_set(Cursor, Value, false).

%% __ret__ cur_set_value/3 ok
%% @docfile "doc/edoc/cur_set_value_3.edoc"
-spec cur_set_value(
        Cursor :: kt_cursor(), 
        Value  :: kt_value(),
        Step   :: boolean()) -> ok
                               | {error, invalid_cursor} 
                               | kt_http_error().

cur_set_value(Cursor, Value, Step) ->
    cur_op_set(Cursor, Value, Step).

cur_op_set(Cursor, Value, Step) ->
    call(Cursor, <<"cur_set_value">>, 
         [{200, ok},
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step),
          arg_kv(<<"value">>, Value)]).

    
%% __ret__ cur_set_value_opt/2 get_signaled_count
%% @docfile "doc/edoc/cur_set_value_opt_2.edoc"
-spec cur_set_value_opt(
        Cursor  :: kt_cursor(), 
        OptArgs :: [{value,       kt_value()}
                 |  {step,        boolean()}
                 |  {xt,          kt_exptime()}
                 |  {wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor} 
                                                | {error, timed_out}
                                                | kt_http_error().

cur_set_value_opt(Cursor, OptArgs) ->
    call(Cursor, <<"cur_set_value">>, 
         [{200, rt_signal()}, 
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{value,    {kv,   <<"value">>}},
             {step,     {flag, <<"step">>}},
             {xt,       {xt,   <<"xt">>}},
             {wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ cur_remove/1 ok
%% @docfile "doc/edoc/cur_remove_1.edoc"
-spec cur_remove(Cursor :: kt_cursor()) -> ok
                                         | {error, invalid_cursor} 
                                         | kt_http_error().

cur_remove(Cursor) ->
    call(Cursor, <<"cur_remove">>, 
         [{200, ok},
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_remove/2 get_signaled_count
%% @docfile "doc/edoc/cur_remove_2.edoc"
-spec cur_remove(Cursor  :: kt_cursor(),
                 OptArgs :: [{wait,        binary() | string()}
                          |  {waittime,    binary() | string() | non_neg_integer()}
                          |  {signal,      binary() | string()}
                          |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                         | {error, invalid_cursor}
                                                         | {error, timed_out}
                                                         | kt_http_error().
cur_remove(Cursor, OptArgs) ->
    call(Cursor, <<"cur_remove">>,
         [{200, rt_signal()},
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).
                 

%% __ret__ cur_get_key/1 get_key
%% @docfile "doc/edoc/cur_get_key_1.edoc"
-spec cur_get_key(Cursor :: kt_cursor()) -> {ok, kt_http_result()}
                                          | {error, invalid_cursor}
                                          | kt_http_error().

cur_get_key(Cursor) ->
    call(Cursor, <<"cur_get_key">>, 
         [{200, rt_key()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_get_key/2 get_key
%% @docfile "doc/edoc/cur_get_key_2.edoc"
-spec cur_get_key(
        Cursor :: kt_cursor(), 
        Step   :: boolean()) -> {ok, kt_http_result()}
                              | {error, invalid_cursor}
                              | kt_http_error().

cur_get_key(Cursor, Step) ->
    call(Cursor, <<"cur_get_key">>,
         [{200, rt_key()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step)]).


%% __ret__ cur_get_key/3 get_key get_signaled_count
%% @docfile "doc/edoc/cur_get_key_3.edoc"
-spec cur_get_key(
        Cursor  :: kt_cursor(), 
        Step    :: boolean(),
        OptArgs :: [{wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor}
                                                | {error, timed_out}
                                                | kt_http_error().

cur_get_key(Cursor, Step, OptArgs) ->
    call(Cursor, <<"cur_get_key">>,
         [{200, rt_key()}, 
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).

%% __ret__ cur_get_value/1 get_value get_exptime
%% @docfile "doc/edoc/cur_get_value_1.edoc"
-spec cur_get_value(Cursor :: kt_cursor()) -> {ok, kt_http_result()}
                                            | {error, invalid_cursor}
                                            | kt_http_error().

cur_get_value(Cursor) ->
    call(Cursor, <<"cur_get_value">>, 
         [{200, rt_value()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_get_value/2 get_value get_exptime
%% @docfile "doc/edoc/cur_get_value_2.edoc"
-spec cur_get_value(
        Cursor :: kt_cursor(), 
        Step   :: boolean()) -> {ok, kt_http_result()}
                              | {error, invalid_cursor}
                              | kt_http_error().

cur_get_value(Cursor, Step) ->
    call(Cursor, <<"cur_get_value">>, 
         [{200, rt_value()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step)]).


%% __ret__ cur_get_value/3 get_value get_exptime get_signaled_count
%% @docfile "doc/edoc/cur_get_value_3.edoc"
-spec cur_get_value(
        Cursor  :: kt_cursor(), 
        Step    :: boolean(),
        OptArgs :: [{wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor}
                                                | {error, timed_out}
                                                | kt_http_error().

cur_get_value(Cursor, Step, OptArgs) ->
    call(Cursor, <<"cur_get_value">>, 
         [{200, rt_value()}, 
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).



%% __ret__ cur_get/1 get_value get_key get_exptime
%% @docfile "doc/edoc/cur_get_1.edoc"
-spec cur_get(Cursor :: kt_cursor()) -> {ok, kt_http_result()}
                                      | {error, invalid_cursor}
                                      | kt_http_error().

cur_get(Cursor) ->
    call(Cursor, <<"cur_get">>, 
         [{200, rt_keyval()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_get/2 get_value get_key get_exptime
%% @docfile "doc/edoc/cur_get_2.edoc"
-spec cur_get(
        Cursor :: kt_cursor(), 
        Step   :: boolean()) -> {ok, kt_http_result()}
                              | {error, invalid_cursor}
                              | kt_http_error().

cur_get(Cursor, Step) ->
    call(Cursor, <<"cur_get">>, 
         [{200, rt_keyval()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step)]).

%% __ret__ cur_get/3 get_value get_key get_exptime get_signaled_count
%% @docfile "doc/edoc/cur_get_3.edoc"
-spec cur_get(
        Cursor  :: kt_cursor(), 
        Step    :: boolean(),
        OptArgs :: [{wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor}
                                                | {error, timed_out}
                                                | kt_http_error().
        
cur_get(Cursor, Step, OptArgs) ->
    call(Cursor, <<"cur_get">>, 
         [{200, rt_keyval()}, 
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_flag(<<"step">>, Step),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).


%% __ret__ cur_seize/1 get_key get_value get_exptime
%% @docfile "doc/edoc/cur_seize_1.edoc"
-spec cur_seize(Cursor::kt_cursor()) -> {ok, kt_http_result()}
                                      | {error, invalid_cursor} 
                                      | kt_http_error().
cur_seize(Cursor) ->
    call(Cursor, <<"cur_seize">>, 
         [{200, rt_keyval()}, 
          {450, {error, invalid_cursor}}],
         [arg_cursor(Cursor)]).

%% __ret__ cur_seize/2 get_key get_value get_exptime
%% @docfile "doc/edoc/cur_seize_2.edoc"
-spec cur_seize(
        Cursor  :: kt_cursor(),
        OptArgs :: [{wait,        binary() | string()}
                 |  {waittime,    binary() | string() | non_neg_integer()}
                 |  {signal,      binary() | string()}
                 |  {signalbroad, boolean()}]) -> {ok, kt_http_result()}
                                                | {error, invalid_cursor}
                                                | {error, timed_out}
                                                | kt_http_error().

cur_seize(Cursor, OptArgs) ->
    call(Cursor, <<"cur_seize">>, 
         [{200, rt_keyval()}, 
          {450, {error, invalid_cursor}},
          {503, {error, timed_out}}],
         [arg_cursor(Cursor),
          arg_multi_opt(
            OptArgs,
            [{wait,     {kv,   <<"WAIT">>}},
             {waittime, {kv,   <<"WAITTIME">>}},
             {signal,   {kv,   <<"SIGNAL">>}},
             {signalbroad, {flag, <<"SIGNALBROAD">>}}])
         ]).


%% =====================================================================
%% binary calls
%% =====================================================================

%% @doc <em>binary protocol:</em> Invokes a procedure of the script language extension.
%% @see play_script/2 
%% @see play_script/3
-spec bin_play_script(
        Client  :: kt_client(),
        Script  :: kt_str(),
        KVL     :: dict() | kt_kv_list()) -> {ok, [kt_bin_rec()]} | {error, term()}.

bin_play_script(Client, Script, KVL) ->
    Rv = [playscript_binrec(K,V) || {K,V} <- kvl(KVL)],
    Scrbin = kterl_util:to_bin(Script),
    Bv = [<<?BIN_MAGIC_PLAY_SCRIPT:8, 
           0:32, 
           (byte_size(Scrbin)):32/big-unsigned-integer,
           (length(Rv)):32/big-unsigned-integer>>,
          Scrbin, Rv],
    case kterl_client:bin_request(Client, Bv) of
        #binary_response{type = play_script, recs = Recs} ->
            {ok, Recs};
        Err = {error, _} -> 
            Err;
        Other ->
            throw({error, {invalid_response, Other}})
    end.

%% @doc <em>binary protocol:</em> Stores multiple k,v pairs at once. Returns the number of records
%% added.
%% @see set_bulk/2
%% @see set_bulk/3
-spec bin_set_bulk(
        Client :: kt_client(),
        Recs   :: [#kt_bin_rec{dbidx :: non_neg_integer(),
                               xt    :: non_neg_integer(),
                               key   :: binary(),
                               val   :: binary()}
                |  {Key :: kt_str(), Value :: kt_str() }
                |  {Key :: kt_str(), Value :: kt_str(), 
                    Database :: non_neg_integer()}
                |  {Key :: kt_str(), Value :: kt_str(), 
                    Database :: non_neg_integer(), 
                    ExpTime :: kt_exptime()}]) -> {ok, RecordsCreated::non_neg_integer()} 
                                                | {error, term()}.

bin_set_bulk(Client, Recs) ->
    Rv = [setbulk_binrec(Rec) || Rec <- bin_kvl(Recs)],
    Bv = [<<?BIN_MAGIC_SET_BULK:8,
           0:32,
           (length(Rv)):32/big-unsigned-integer>> |
          Rv],
    case kterl_client:bin_request(Client, Bv) of
        #binary_response{type = set_bulk, tot_recs = TR} ->
            {ok, TR};
        Err = {error, _} ->
            Err;
        Other ->
            throw({error, {invalid_response, Other}})
    end.

%% @doc <em>binary protocol:</em> Removes multiple records at once. Returns the number of
%% record deletions performed.
%% @see remove_bulk/2
%% @see remove_bulk/3
-spec bin_remove_bulk(
        Client :: kt_client(),
        Keys   :: kt_key_list() | {Key::kt_key(), Database::non_neg_integer()})
                     -> {ok, RecsDeleted::non_neg_integer()} | {error, term()}.

bin_remove_bulk(Client, Keys) ->
    Rv = [getbulk_binrec(Rec) || Rec <- Keys],
    Bv = [<<?BIN_MAGIC_REMOVE_BULK:8,
           0:32,
           (length(Keys)):32/big-unsigned-integer>> |
          Rv],
    case kterl_client:bin_request(Client, Bv) of
        #binary_response{type = remove_bulk, tot_recs = TR} ->
            {ok, TR};
        Err = {error, _} ->
            Err;
        Other ->
            throw({error, {invalid_response, Other}})
    end.

%% @doc Retrieves multiple records using the binary protocol.
%% @see get_bulk/2
%% @see get_bulk/3
-spec bin_get_bulk(
        Client :: kt_client(),
        Keys   :: kt_key_list() | {Key::kt_key(), Database::non_neg_integer()})
                  -> {ok, [kt_bin_rec()]} | {error, term()}.

bin_get_bulk(Client, Keys) ->
    Rv = [getbulk_binrec(Rec) || Rec <- Keys],
    Bv = [<<?BIN_MAGIC_GET_BULK:8,
           0:32,
           (length(Keys)):32/big-unsigned-integer>> |
          Rv],
    case kterl_client:bin_request(Client, Bv) of
        #binary_response{type = get_bulk, recs = Recs} ->
            {ok, Recs};
        Err = {error, _} ->
            Err;
        Other ->
            throw({error, {invalid_response, Other}})
    end.

%% =====================================================================
%%
%% =====================================================================

-spec call(
        Client  :: kt_client() | kt_cursor(),
        Command :: binary(),
        Results :: result_proc()) -> ok 
                                   | {ok, kt_http_result()}  
                                   | {error, term()}.

call(Client, Command, Results) ->
    call(Client, Command, Results, []).
                  

-spec call(
        Client  :: kt_client() | kt_cursor(),
        Command :: binary(),
        Results :: result_proc(),
        Body    :: iolist()) -> ok 
                              | {ok, kt_http_result()}
                              | {error, term()}.

call(Client, Command, Results, Body) ->
    Bodylen = iolist_size(Body),
    Pid = client_pid(Client),
    Callres = kterl_client:http_request(Pid, Command, Body, Bodylen),
    handle_response(Command, Callres, Results).

handle_response(
  Command, 
  #http_response{status_code = Code, body = Body, response_enc = Enc},
  Results) ->
    case lists:keyfind(Code, 1, Results) of
        {Code, F} when is_function(F) ->
            F(Code, Body, Enc);
        {Code, ok} -> ok;
        {Code, Err = {error,_}} -> Err;
        false ->
            throw({error, {unhandled_result, Command, Code}})
    end;
handle_response(_, Err = {error, _}, _) ->
    Err.

             
%% =====================================================================
%%
%% =====================================================================

%% this function is called for report/status.
%% no need to check for signaled; no special handling needed
%% on results.
rt_kvl() ->
    fun rt_kvl/3.
rt_kvl(_StatusCode, Body, EncType) ->
    Recs = body_to_kv(Body, EncType),
    {ok, #kt_http_result{call_type = rt_kvl,
                         bulk_records = Recs}}.

%% this function is for calls that return _key\tvalue
%% where database keys are prefixed with an underscore.
rt_ukvl() ->
    fun rt_ukvl/3.
rt_ukvl(_StatusCode, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    Recs = filter_ukv(KV),
    Signaled = get_signaled(KV),
    Num = get_num(KV),
    %% might want to compare Num with the # of 
    %% results that were actually received from the server.
    {ok, #kt_http_result{call_type = ukvl,
                         num = Num,
                         signaled_count = Signaled,
                         bulk_records = Recs}}.

%% this function is used for match_prefix | match_regex.
rt_ukey() ->
    fun rt_ukey/3.
rt_ukey(_StatusCode, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    Keys = filter_uk(KV),
    Num = get_num(KV),
    %% might want to compare Num with the # of results
    %% that were actually received from the server.
    Signaled = get_signaled(KV),
    {ok, #kt_http_result{call_type = ukey,
                         num = Num,
                         signaled_count = Signaled,
                         keys = Keys}}.

%% this function is for calls that return a 'num' as their
%% primary result... set_bulk | increment 
rt_num() ->
    fun rt_num/3.
rt_num(_, Body, EncType) ->
    rt_gen_num(num, Body, EncType).

%% this function is for calls that return a floating point 'num'
%% as its primary result.. increment_double
rt_float() ->
    fun rt_float/3.
rt_float(_, Body, EncType) ->
    rt_gen_num(float, Body, EncType).


rt_gen_num(CallType, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    case lists:keyfind(<<"num">>, 1, KV) of
        {_, NumB} when is_binary(NumB) ->
            NumL = binary_to_list(NumB),
            Num = case CallType of
                      float -> list_to_float(NumL);
                      num -> list_to_integer(NumL)
                  end,
            Signaled = get_signaled(KV),
            {ok, #kt_http_result{call_type = CallType,
                                 num = Num,
                                 signaled_count = Signaled}};
        false ->
            {error, no_num}
    end.
            

%% this function is for calls that return a value for a given key,
%% or cursor calls. get | seize | cur_get_value
rt_value() ->
    fun rt_value/3.
rt_value(_, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    case lists:keyfind(<<"value">>, 1, KV) of
        {_, Value} -> 
            Signaled = get_signaled(KV),
            XT = get_xt(KV),
            {ok, #kt_http_result{call_type = value,
                                 signaled_count = Signaled,
                                 exptime = XT,
                                 value = Value}};
        false ->
            {error, no_value}
    end.

%% this function is for cursor calls that return the key of the
%% existing cursor's position.
rt_key() ->
    fun rt_key/3.
rt_key(_, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    case lists:keyfind(<<"key">>, 1, KV) of
        {_, Key} -> 
            Signaled = get_signaled(KV),
            {ok, #kt_http_result{call_type = key,
                                 signaled_count = Signaled,
                                 key = Key}};
        false -> {error, no_key}
    end.

%% this is for calls that return both key and value.
%% only cur_get and cur_seize utilize it.
rt_keyval() ->
    fun rt_keyval/3.
rt_keyval(_, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    case get_key_value(KV) of
        {Key, Value} ->
            XT = get_xt(KV),
            Signaled = get_signaled(KV),
            {ok, #kt_http_result{call_type = keyval,
                                 key = Key,
                                 value = Value,
                                 signaled_count = Signaled,
                                 exptime = XT}};
        Missing when is_atom(Missing) -> {error, Missing}
    end.

%% this is for functions that only need to check for
%% the existence of the signaled value in the response.
rt_signal() ->
    fun rt_signal/3.
rt_signal(_, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    Signaled = get_signaled(KV),
    {ok, #kt_http_result{call_type = Signaled,
                         signaled_count = Signaled}}.

rt_error() ->
    fun rt_error/3.
rt_error(StatusCode, Body, EncType) ->
    KV = body_to_kv(Body, EncType),
    Err = proplists:get_value(<<"ERROR">>, KV, KV),
    {error, {http_result, StatusCode, Err}}.


%% return {Key, Value} from [{<<"key">>,Key}, {<<"value">>,Value}, ...]
%% lists:keyfind() is a BIF
-spec get_key_value(kt_bin_kv_list()) -> no_key | no_value | 
                                         no_key_value | kt_bin_kv(). 
get_key_value(L) ->
    KF = fun(S) -> 
                 case lists:keyfind(S, 1, L) of
                     {_, V} when is_binary(V) -> V;
                     false  -> undefined
                 end
         end,
    case {KF(<<"key">>), KF(<<"value">>)} of
        KV = {Key, Value} when is_binary(Key), is_binary(Value) -> KV;
        {Key, Value} ->
            list_to_atom("no" ++
                             case Key of undefined -> "_key"; _ -> [] end ++
                             case Value of undefined -> "_value"; _ -> [] end)
    end.

-spec get_signaled(kt_bin_kv_list()) -> integer() | undefined.
get_signaled(PL) ->
    gen_get_k(PL, <<"SIGNALED">>, fun kterl_util:to_int/1).

-spec get_xt(kt_bin_kv_list()) -> non_neg_integer() | undefined.
get_xt(PL) ->
    gen_get_k(PL, <<"xt">>, fun kterl_util:to_int/1).

-spec get_num(kt_bin_kv_list()) -> integer() | undefined.
get_num(PL) -> 
    gen_get_k(PL, <<"num">>, fun kterl_util:to_int/1).

gen_get_k(PL, Key, Convfun) ->
    case lists:keyfind(Key, 1, PL) of
        {_, V} when is_binary(V) ->
            Convfun(V);
        false ->
            undefined
    end.


%% =====================================================================
%%
%% =====================================================================


-spec body_to_kv(Body::binary(), EncType::response_enc()) -> [] | kt_kv_list().

body_to_kv(Body, EncType) ->
    kterl_util:bin_split_decode_kv(Body, EncType).

-spec arg_multi_opt(
        PL :: kt_optargs(),
        [{PLKey :: atom(), {ValType :: 'xt' | 'kv' | 'flag',
                            BinKey :: binary()}}]) -> iolist().

arg_multi_opt(_, []) -> [];
arg_multi_opt(PL, [{PLKey, {ValType, BinKey}} | T]) ->
    EFun = case ValType of
               flag -> fun arg_flag/2;
               kv   -> fun arg_kv/2;
               xt   -> fun arg_xt/2
           end,
    case lists:keyfind(PLKey, 1, PL) of
        {_, Value} -> [EFun(BinKey, Value) | arg_multi_opt(PL, T)];
        false -> arg_multi_opt(PL, T)
    end.

-spec arg_flag(Key::binary(), Value::boolean()) -> binary().

arg_flag(Key, Value) ->
    case Value of
        true -> build_k(Key);
        false -> <<>>
    end.

-spec arg_xt(Key::binary(), Value :: kt_exptime()) -> binary().
    
arg_xt(Key, Val) -> 
    arg_kv(Key, get_exptime(Val)).

-spec arg_cursor(Cursor::kt_cursor()) -> binary().

arg_cursor(Cursor) ->
    build_kv(<<"CUR">>, cursor_id(Cursor)).

-spec arg_kv(Key::kt_key(), Val::kt_value()) -> binary().

arg_kv(Key, Value) -> build_kv(Key, Value).

-spec arg_kvl(KVL::kt_kv_list()) -> iolist().

arg_kvl(KVL) -> [build_kv(KV) || KV <- kvl(KVL)].

-spec arg_ukeys(Keys::kt_key_list()) -> iolist().

arg_ukeys(Keys) -> [build_uk(Key) || Key <- Keys].

-spec arg_ukvl(KVL::kt_kv_list()) -> iolist().
    
arg_ukvl(KVL) -> [build_ukv(KV) || KV <- kvl(KVL)].


%% =====================================================================
%%
%% =====================================================================

-spec client_pid(pid() | kt_client() | kt_cursor()) -> pid().

client_pid(Pid) when is_pid(Pid) -> Pid;
client_pid(#kterl_cursor{client_pid = Pid}) -> client_pid(Pid).


-spec cursor_id(kt_cursor()) -> non_neg_integer() | no_return().

cursor_id(#kterl_cursor{cursor_id = C}) when is_integer(C) -> C;
cursor_id(C) -> throw({error, {cursor_id, invalid_cursor, C}}).


%% =====================================================================
%%
%% =====================================================================

%% binary protocol only supports integer database specifier.
-spec getbulk_binrec( 
        Key::kt_key() 
      | {Key::kt_key(), Database::non_neg_integer()}) -> iolist().

getbulk_binrec(K) when is_list(K); is_binary(K) -> getbulk_binrec({K,0});
getbulk_binrec({K, DB}) 
  when (is_list(K) orelse is_binary(K)) andalso is_integer(DB) ->
    Kbin = kterl_util:to_bin(K),
    [<<DB:16/big-unsigned-integer,
      (byte_size(Kbin)):32/big-unsigned-integer>>,
     Kbin].

-spec playscript_binrec(Key::kt_key(), Value::kt_value()) -> iolist().

playscript_binrec(K,V) ->
    Kbin = kterl_util:to_bin(K),
    Vbin = kterl_util:to_bin(translate_value(V)),
    [<<(byte_size(Kbin)):32/big-unsigned-integer,
       (byte_size(Vbin)):32/big-unsigned-integer>>,
     Kbin, Vbin].

-spec setbulk_binrec(
        kt_kv()
        | #kt_bin_rec{}
        | {Key::kt_key(), Value::kt_value(), Database::non_neg_integer()}
        | {Key::kt_key(), Value::kt_value(), Database::non_neg_integer(), 
           Exptime::kt_exptime()}) -> iolist().

setbulk_binrec(#kt_bin_rec{key = Key, val = Val, dbidx = Db, xt = Xt}) ->
    setbulk_binrec({Key,Val,Db,Xt});
setbulk_binrec({K,V}) -> setbulk_binrec({K,V,0,?DEFAULT_XT});
setbulk_binrec({K,V,DB}) -> setbulk_binrec({K,V,DB,?DEFAULT_XT});
setbulk_binrec({K,V,DBin,XTin}) ->
    Kbin = kterl_util:to_bin(K),
    Vbin = kterl_util:to_bin(translate_value(V)),
    XT = case XTin of
             undefined -> ?DEFAULT_XT;
             Other -> get_exptime(Other)
         end,
    DB = case DBin of
             N when is_integer(N), N >= 0 -> N;
             undefined -> 0;
             _ -> throw({error,invalid_database})
         end,
    [<<DB:16/big-unsigned-integer,
      (byte_size(Kbin)):32/big-unsigned-integer,
      (byte_size(Vbin)):32/big-unsigned-integer,
      XT:64/big-unsigned-integer>>,
     Kbin, Vbin].

%% =====================================================================
%%
%% =====================================================================

-spec kvl(KVL  :: kt_kv_list()) -> kt_kv_list()
       ; (Dict :: dict()) -> kt_kv_list().

%kvl(Dict) when is_tuple(Dict) andalso element(1,Dict) =:= dict -> dict:to_list(Dict).
kvl(KVL = [{_,_}|_]) -> KVL;
kvl(Dict) -> dict_to_list(Dict).

bin_kvl(R = [{_K,_V} | _]) -> R;
bin_kvl(R = [{_K,_V,_D} | _]) -> R;
bin_kvl(R = [{_K,_V,_D,_E} | _]) -> R;
bin_kvl(Dict) -> dict_to_list(Dict).

dict_to_list(Dict) ->
    try dict:to_list(Dict)
    catch error:{badrecord, dict} -> throw({error, {invalid_kv, Dict}})
    end.
             

-spec get_exptime(V :: kt_exptime()) -> non_neg_integer().

get_exptime(V) ->
    Ret = case V of
              N when is_integer(N) -> 
                  N;
              DateTime when is_tuple(DateTime) -> 
                  kterl_util:future_seconds(DateTime)
          end,

    if Ret > 0 -> Ret;
       true -> throw({error, {invalid_exptime, V, Ret}})
    end.


%% @doc convert common erlang types to string|binary
-spec translate_value(kt_value()) -> kt_str().
translate_value(L) when is_list(L) -> unicode:characters_to_binary(L);
translate_value(S) when is_binary(S) -> S;
translate_value(I) when is_integer(I) -> integer_to_list(I);
translate_value(F) when is_float(F) -> 
    case io_lib:format("~f",[F]) of
        [FStr] when is_list(FStr) -> FStr;
        Err -> throw({error,{translate_float,F,Err}})
    end;
translate_value(A) when is_atom(A) -> atom_to_list(A).


-spec build_k(kt_str()) -> binary().
build_k(K) ->
    Kenc = base64:encode(translate_value(K)),
    << Kenc/binary, "\t\n">>.


-spec build_uk(kt_str()) -> binary().
build_uk(K) ->
    build_k(force_uscore(K)).


-spec build_kv({kt_str(), kt_value()}) -> binary().
build_kv({K,V}) -> build_kv(K,V).


-spec build_kv(kt_str(), kt_value()) -> binary().
build_kv(K, V) ->
    Kenc = base64:encode(translate_value(K)),
    Venc = base64:encode(translate_value(V)),
    << Kenc/binary, "\t", Venc/binary, "\n">>.


-spec build_ukv({kt_str(), kt_str()}) -> binary().
build_ukv({K,V}) -> build_ukv(K,V).


-spec build_ukv(kt_str(), kt_str()) -> binary().
build_ukv(K,V) ->   build_kv(force_uscore(K), V).


-spec force_uscore(kt_str()) -> kt_str().
force_uscore(L = [$_|_]) -> L;
force_uscore(L) when is_list(L) -> [$_ | L];
force_uscore(B = <<$_, _/binary>>) -> B;
force_uscore(B) when is_binary(B) -> <<$_, B/binary>>.


-spec filter_ukv(BPL) -> BPL 
      when BPL :: kt_bin_kv_list().
filter_ukv(L) -> [{Key,Value} || {<<$_, Key/binary>>, Value} <- L].


-spec filter_uk(kt_bin_kv_list()) -> [binary()].
filter_uk(L) -> [Key || {<<$_, Key/binary>>, _} <- L].
