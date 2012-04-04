%% @private
%% first start on localhost: ktserver %
-module(kterl_tests).

-include_lib("eunit/include/eunit.hrl").
-include("kterl.hrl").

-define(TEST_PORT,   1978).
-define(N_BULK_KEYS, 10).

-ifdef(todo).

-record(test_state, { 
          ktc_pid    :: pid(),
          server_pid :: pid()
         }).

launch_ktserver() ->
    Port = open_port({spawn,"/usr/local/bin/ktserver -port " ++ integer_to_list(?TEST_PORT)},
                     [binary,{line,255}]),
    ktserver_loop(Port).
ktserver_loop(Port) ->
    receive 
        {Port, {data, Data}} ->
            error_logger:info_msg("KTSERVER: ~p~n",[Data]);
        {Port,eof} ->
            error_logger:error_msg("KTSERVER EOF");
        Noise ->
            error_logger:error_msg("KTSERVER NOISE ~p~n",[Noise])
    end,
    ktserver_loop(Port).

ktc(#test_state{ktc_pid = P}) -> P.

-endif.

connect() ->
    %Kts = spawn_link(fun() -> launch_ktserver() end),
    {ok,C} = kterl:start_link([{host, "127.0.0.1"}, {port,?TEST_PORT}]),
    C.

disconnect(C) ->
    ok = kterl:stop(C).



kterl_test_() ->
    {foreach,
     fun connect/0,
     fun disconnect/1,
     [fun wire_dump/1
      ,fun void/1
      ,fun echo/1
      ,fun status/1
      ,fun report/1
      ,fun clear/1
      ,fun add_set_get/1
      ,fun append/1
      ,fun replace/1
      ,fun set_bulk/1
      ,fun get_bulk/1
      %,fun get_bulk_atomic/1
      ,fun remove_bulk/1
      ,fun clear/1
      ,fun cas/1
      ,fun increment/1
      ,fun increment_double/1
      ,fun set_integer_value/1
      ,fun set_float_value/1
      ,fun erlang_terms/1
      ,fun cursor/1
      ,fun cursor_invalidation/1
      ,fun cursor_step_forward/1
      ,fun cursor_step_backward/1
      ,fun set_get_big_keys/1
      ,fun set_get_big_values/1
      ,fun exptime/1
      ,fun clear/1
      ,fun vacuum/1
      ]}.

wire_dump(C) ->
    [?_assertEqual(ok, kterl:configure(C, [{wire_dump, true}])),
     ?_assertEqual(ok, kterl:configure(C, [{wire_dump, false}]))].

void(C) ->
    [?_assertEqual(ok, kterl:void(C))].

echo(C) ->
    %% server appears to reorder k/v
    MSG = [{<<"hello">>, <<"world">>},
           {<<"this">>,  <<"is">>},
           {<<"a">>,     <<"test">>}],
    {ok,Res} = kterl:echo(C, MSG),
    R = kterl_result:get_records(Res),
    [?_assertEqual(lists:sort(R), lists:sort(MSG))].

status(C) ->
    [?_assertMatch(L when is_list(L), recs(kterl:status(C)))].

report(C) ->
    [?_assertMatch(L when is_list(L), recs(kterl:report(C)))].

clear(C) ->
    [?_assertMatch(ok, kterl:clear(C))].

add_set_get(C) ->
    [?_assertMatch(ok,kterl:add(C,<<"testkey1">>,<<"testval1">>))
     ,?_assertEqual({error, duplicate_key}, kterl:add(C,"testkey1","value"))
     ,?_assertEqual(<<"testval1">>,kterl_result:get_value(kterl:get(C,<<"testkey1">>)))
     ,?_assertMatch(ok,kterl:set(C,"testkey1",<<"value updated!">>))
     ,?_assertEqual(<<"value updated!">>,kterl_result:get_value(kterl:get(C,"testkey1")))
     ,?_assertMatch(ok,kterl:remove(C,<<"testkey1">>))
    ].

append(C) ->
    K = <<"appendtest1">>,
    kterl:remove(C, K),
    [?_assertEqual(ok, ok(kterl:append(C,K,<<>>)))
     ,?_assertEqual(<<>>, kterl_result:get_value(kterl:get(C,K)))
     ,?_assertEqual(ok, ok(kterl:append(C,K,<<"Hello World!">>)))
     ,?_assertEqual(<<"Hello World!">>, kterl_result:get_value(kterl:get(C,K)))
     ,?_assertEqual(ok, ok(kterl:append(C,K,<<" test1234">>)))
     ,?_assertEqual(<<"Hello World! test1234">>, kterl_result:get_value(kterl:get(C,K)))
     ,?_assertEqual(ok, ok(kterl:remove(C,K)))
     ,?_assertEqual({error, no_record}, kterl:get(C,K))
    ].

% quick call success
ok({ok,_}) -> ok;
ok(O) ->  O.

val({ok,R}) -> kterl_result:get_value(R);
val(O) -> O.

key({ok,R}) -> kterl_result:get_key(R);
key(O) -> O.

num({ok,R}) -> kterl_result:get_num(R);
num(O) -> O.

recs({ok,R}) -> kterl_result:get_records(R);
recs(O) -> O.

rec({ok,R}) -> 
    K = kterl_result:get_key(R),
    V = kterl_result:get_value(R),
    {K,V}.

replace(C) ->
    K = <<"replace test #1">>,
    R1 = <<"This is a test...">>,
    R2 = <<"This is only a test!">>,
    [?_assertEqual({error, no_record}, kterl:replace(C,K,<<>>))
     ,?_assertEqual(ok, ok(kterl:add(C,K,<<>>)))
     ,?_assertEqual(<<>>, val(kterl:get(C,K)))
     ,?_assertEqual(ok, ok(kterl:replace(C,K,R1)))
     ,?_assertEqual(R1, val(kterl:get(C,K)))
     ,?_assertEqual(ok, ok(kterl:replace(C,K,R2)))
     ,?_assertEqual(R2, val(kterl:get(C,K)))
     ,?_assertEqual(R2, val(kterl:seize(C,K)))
     ,?_assertEqual({error, no_record}, kterl:get(C,K))
    ].

set_bulk(C) ->
    [?_assertEqual(?N_BULK_KEYS,num(kterl:set_bulk(C,bulk_kv_string_recs(?N_BULK_KEYS))))
     ,?_assertEqual(?N_BULK_KEYS,num(kterl:set_bulk(C,bulk_kv_bin_recs(?N_BULK_KEYS),[{atomic, true}])))
     ,?_assertEqual(ok,ok(kterl:clear(C)))
     ,?_assertEqual(?N_BULK_KEYS, num(kterl:set_bulk(C,dict:from_list(bulk_kv_string_recs(?N_BULK_KEYS)))))
     ,?_assertEqual(?N_BULK_KEYS, num(kterl:set_bulk(C,dict:from_list(bulk_kv_bin_recs(?N_BULK_KEYS)))))
    ].

get_bulk_optargs(C,OptArgs) ->
    StringRecs = bulk_kv_string_recs(?N_BULK_KEYS),
    BinRecs = bulk_kv_bin_recs(?N_BULK_KEYS),
    StringKeys = [Key || {Key, _} <- StringRecs],
    BinKeys = [Key || {Key, _} <- BinRecs],
    SKRes = kterl_result:get_records( kterl:get_bulk(C, StringKeys, OptArgs)),
    BKRes = kterl_result:get_records( kterl:get_bulk(C, BinKeys, OptArgs)),
    [?_assertEqual(length(SKRes), length(StringKeys))
     ,?_assertEqual(length(BKRes), length(BinKeys))
     ,?_assertEqual(lists:sort([{binary_to_list(K),binary_to_list(V)} || {K,V} <- SKRes]), 
                    lists:sort(StringRecs))
     ,?_assertEqual(lists:sort(BKRes), lists:sort(BinRecs))
    ].
    
get_bulk(C) ->
    get_bulk_optargs(C,[]).

%% get_bulk_atomic(C) ->
%%     StringRecs = bulk_kv_string_recs(?N_BULK_KEYS),
%%     BinRecs = bulk_kv_bin_recs(?N_BULK_KEYS),
%%     StringKeys = [Key || {Key, _} <-StringRecs],
%%     BinKeys = [Key || {Key, _} <- BinRecs],
%%     [_|MissingStringKeys] = StringKeys,
%%     [_|MissingBinKeys] = BinKeys,
%%     [?_assertEqual(?N_BULK_KEYS, length(kterl:get_bulk(C, StringKeys, [{atomic,true}])))
%%      ,?_assertEqual(?N_BULK_KEYS, length(kterl:get_bulk(C, BinKeys,   [{atomic,true}])))
%%      ,?_assertEqual([], kterl:get_bulk(C, MissingStringKeys, [{atomic,true}]))
%%      ,?_assertEqual([], kterl:get_bulk(C, MissingBinKeys, [{atomic,true}]))
%%     ].
     
remove_bulk(C) ->    
    StringRecs = bulk_kv_string_recs(?N_BULK_KEYS),
    BinRecs = bulk_kv_bin_recs(?N_BULK_KEYS),
    StringKeys = [Key || {Key,_} <- StringRecs],
    BinKeys = [Key || {Key,_} <- BinRecs],
    [ ?_assertMatch(L when length(L) == ?N_BULK_KEYS, recs(kterl:get_bulk(C, StringKeys)))
      ,?_assertMatch(L when length(L) == ?N_BULK_KEYS, recs(kterl:get_bulk(C, BinKeys)))
      ,?_assertEqual(?N_BULK_KEYS, num(kterl:remove_bulk(C, StringKeys)))
      ,?_assertEqual(?N_BULK_KEYS, num(kterl:remove_bulk(C, BinKeys, [{atomic, true}])))
      ,?_assertEqual([], recs(kterl:get_bulk(C, StringKeys)))
      ,?_assertEqual([], recs(kterl:get_bulk(C, BinKeys)))
    ].

cas(C) ->
    K = <<"cas test #1">>,
    V1 = <<"FiRsT vAlUe!">>,
    V2 = <<"sEcOnD vAlUe...">>,
    kterl:remove(C, K),
    [ ?_assertEqual({error, expired_value}, kterl:cas(C, K, <<>>, V1))
      ,?_assertEqual(ok, ok(kterl:set(C, K, V1)))
      ,?_assertEqual(V1, val(kterl:get(C, K)))
      ,?_assertEqual({error, expired_value}, kterl:cas(C, K, <<>>, V2))
      ,?_assertEqual(ok, ok(kterl:cas(C, K, [{oval, V1}, {nval, V2}])))
      ,?_assertEqual(V2, val(kterl:get(C, K)))
      ,?_assertEqual(V2, val(kterl:seize(C, K)))
      ,?_assertEqual({error, no_record}, kterl:get(C, K))
    ].

increment(C) ->
    [ ?_assertEqual(ok,         ok(kterl:clear(C)))
      ,?_assertEqual(1,         num(kterl:increment(C,"incr_test",1)))
      ,?_assertEqual(2,         num(kterl:increment(C,<<"incr_test">>,1)))
      ,?_assertEqual(0,         num(kterl:increment(C,"incr_test",-2)))
      ,?_assertEqual(1000,      num(kterl:increment(C,"incr_test",1000)))
      ,?_assertEqual(-500,      num(kterl:increment(C,"incr_test",-1500)))
      ,?_assertEqual(-1,        num(kterl:increment(C,"incr_test",499)))
      ,?_assertEqual(1,         num(kterl:increment(C,"incr_test",2)))
      ,?_assertEqual(2,         num(kterl:increment(C,"incr_test",1)))
      ,?_assertEqual(0,         num(kterl:increment(C,<<"incr_test">>,-2)))
      ,?_assertEqual(ok,        ok(kterl:remove(C,<<"incr_test">>)))
     ].

increment_double(C) ->
    [ ?_assertEqual(ok,         ok(kterl:clear(C)))
      ,?_assertEqual(1.0,       num(kterl:increment_double(C, "incrd_test", 1.0)))
      ,?_assertEqual(1.1,       num(kterl:increment_double(C, "incrd_test", 0.1)))
      ,?_assertEqual(0.0,       num(kterl:increment_double(C, <<"incrd_test">>, -1.1)))
      ,?_assertEqual(123.456,   num(kterl:increment_double(C, "incrd_test", 123.456)))
      ,?_assertEqual(-123.456,  num(kterl:increment_double(C, "incrd_test", -123.456*2)))
      ,?_assertEqual(1.0,       num(kterl:increment_double(C, "incrd_test", 124.456)))
      ,?_assertEqual(ok,        ok(kterl:remove(C,<<"incrd_test">>)))
     ].

set_integer_value(C) ->
    [ ?_assertEqual(ok, ok(kterl:set(C,<<"int_value">>, 123456789)))
      ,?_assertEqual(<<"123456789">>, val(kterl:get(C,"int_value")))
      ,?_assertEqual(ok, ok(kterl:remove(C, <<"int_value">>)))].

set_float_value(C) ->
    F = fun(K) ->
                V = kterl_result:get_value(kterl:get(C,K)),
                list_to_float(binary_to_list(V))
        end,
    [ ?_assertEqual(ok, ok(kterl:set(C, <<"float_value">>, 123456.789)))
      ,?_assertEqual(123456.789, F(<<"float_value">>))
      ,?_assertEqual(ok, ok(kterl:remove(C, "float_value")))].

erlang_terms(C) ->
    T = [{hello, "Hello!"}, {<<"world">>, 123456789000}, {testing, 123456.789}],
    BT = term_to_binary(T,[compressed]),
    KT = {key,1234,5678,900,<<"testing">>},
    BK = term_to_binary(KT),
    
    F = fun(K) ->
                Term = kterl_result:get_value(kterl:get(C,K)),
                binary_to_term(Term)
           end,

    [ ?_assertEqual(ok, ok(kterl:set(C, <<"eterm1">>, BT)))
      ,?_assertEqual(BT, val(kterl:get(C, <<"eterm1">>)))
      ,?_assertEqual(T, F("eterm1"))
      ,?_assertEqual(ok, ok(kterl:remove(C, <<"eterm1">>)))
      ,?_assertEqual(ok, ok(kterl:set(C, BK, BT)))
      ,?_assertEqual(BT, val(kterl:get(C, BK)))
      ,?_assertEqual(T, F(BK))
      ,?_assertEqual(ok, ok(kterl:remove(C, BK)))
     ].

cursor(C) ->
    %% Note: success of these tests depends on ordering provided by the
    %% tree database
    Recs = [{"key_" ++ [AtD], "val_" ++ [AtD]} || AtD <- lists:seq($a,$d)],
    Cursor = kterl:cursor(C),
    [ ?_assertEqual(ok, ok(kterl:clear(C)))
      ,?_assertEqual(4, num(kterl:set_bulk(C, Recs)))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor, [{key,"key_a"}])))
      ,?_assertEqual(<<"val_a">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual({<<"key_a">>, <<"val_a">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(<<"key_a">>, key(kterl:cur_get_key(Cursor)))
      %% jump to next, repeat
      ,?_assertEqual(ok, ok(kterl:cur_step(Cursor)))
      ,?_assertEqual(<<"val_b">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual(<<"key_b">>, key(kterl:cur_get_key(Cursor)))
      ,?_assertEqual({<<"key_b">>, <<"val_b">>}, rec(kterl:cur_get(Cursor)))
      %%
      ,?_assertEqual(ok, ok(kterl:cur_jump_back(Cursor)))
      ,?_assertEqual(<<"val_d">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual(<<"key_d">>, key(kterl:cur_get_key(Cursor)))
      ,?_assertEqual({<<"key_d">>, <<"val_d">>}, rec(kterl:cur_get(Cursor)))
      %%
      ,?_assertEqual(ok, ok(kterl:cur_step_back(Cursor)))
      ,?_assertEqual({<<"key_c">>, <<"val_c">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_step_back(Cursor)))
      ,?_assertEqual({<<"key_b">>, <<"val_b">>}, rec(kterl:cur_get(Cursor)))
      %% bounds
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual({<<"key_a">>, <<"val_a">>}, rec(kterl:cur_get(Cursor)))
      %% as expected, this call fails.. it also invalidates the cursor.
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_step_back(Cursor))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_remove(Cursor)))
      ,?_assertEqual({<<"key_b">>, <<"val_b">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_set_value(Cursor, <<"new_val_b">>)))
      ,?_assertEqual({<<"key_b">>, <<"new_val_b">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_set_value(Cursor, <<"newest_val_b">>)))
      ,?_assertEqual(<<"newest_val_b">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual({<<"key_b">>, <<"newest_val_b">>}, rec(kterl:cur_seize(Cursor)))
      ,?_assertEqual(<<"val_c">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual(<<"key_c">>, key(kterl:cur_get_key(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_jump_back(Cursor)))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_step(Cursor))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor, [{key,"key_d"}])))
      ,?_assertEqual({<<"key_d">>, <<"val_d">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual({<<"key_d">>, <<"val_d">>}, rec(kterl:cur_seize(Cursor)))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_get(Cursor))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_step_back(Cursor))
      ,?_assertEqual(ok, ok(kterl:cur_jump_back(Cursor)))
      %,?_assertEqual(ok, kterl:cur_jump(Cursor, "key_c")) -- this works, too.
      ,?_assertEqual({<<"key_c">>, <<"val_c">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_remove(Cursor)))
      %% no more records in database
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_jump(Cursor))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_jump_back(Cursor))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_jump(Cursor, [{key,<<"key_c">>}]))
      ,?_assertEqual(ok, ok(kterl:release_cursor(Cursor)))
    ].

cursor_invalidation(C) ->
    Recs = [{"key_" ++ [AtD], "val_" ++ [AtD]} || AtD <- lists:seq($a, $z)],
    Cursor = kterl:cursor(C),
    [ ?_assertEqual(ok, ok(kterl:clear(C)))
      ,?_assertEqual(26, num(kterl:set_bulk(C, Recs)))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual({<<"key_a">>, <<"val_a">>}, rec(kterl:cur_get(Cursor)))
      ,?_assertEqual(ok, ok(kterl:cur_jump_back(Cursor)))
      ,?_assertEqual(<<"key_z">>, key(kterl:cur_get_key(Cursor)))
      ,?_assertEqual(<<"val_z">>, val(kterl:cur_get_value(Cursor)))
      ,?_assertEqual(ok, ok(kterl:release_cursor(Cursor)))
      ,?_assertEqual({error, invalid_cursor}, kterl:cur_get(Cursor))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual(<<"key_a">>, key(kterl:cur_get_key(Cursor)))
      ,?_assertEqual(ok, ok(kterl:release_cursor(Cursor)))
      ,?_assertEqual(ok, ok(kterl:clear(C)))
    ].

cursor_step_forward(C) ->
    Recs = bulk_kv_bin_recs(?N_BULK_KEYS),
    Cursor = kterl:cursor(C),
    [ ?_assertEqual(ok, ok(kterl:clear(C)))
      ,?_assertEqual(?N_BULK_KEYS, num(kterl:set_bulk(C, Recs)))
      ,?_assertEqual(ok, ok(kterl:cur_jump(Cursor)))
      ,?_assertEqual({ok,?N_BULK_KEYS}, cursor_sweep(Cursor, Recs))
    ].

cursor_step_backward(C) ->
    Recs = bulk_kv_bin_recs(?N_BULK_KEYS),
    Cursor = kterl:cursor(C),
    [ ?_assertEqual(ok, ok(kterl:clear(C)))
      ,?_assertEqual(?N_BULK_KEYS, num(kterl:set_bulk(C, Recs)))
      ,?_assertEqual(ok, ok(kterl:cur_jump_back(Cursor)))
      ,?_assertEqual({ok,?N_BULK_KEYS}, cursor_sweep(Cursor, Recs))
      ,?_assertEqual(ok, ok(kterl:clear(C)))
    ].

set_get_big_keys(C) ->
    K64B = big_binary(1 bsl 6),
    K128B = big_binary(1 bsl 7),
    K256B = big_binary(1 bsl 8),
    K1K = big_binary(1 bsl 10),
    K2K = big_binary(1 bsl 11),
    K64K  = big_binary(1 bsl 16),
    K256K = big_binary(1 bsl 18),
    K512K = big_binary(1 bsl 19),
    K1M = big_binary(1 bsl 20),
    Value64K = big_binary(1 bsl 16),
    Keys = [K64B, K128B, K256B, K1K, K2K, K64K, K256K, K512K, K1M],
    KV = [{K,V} || K <- Keys, V <- [Value64K]],
    CompareBulk = fun() ->
                          {ok,Res} = kterl:get_bulk(C, Keys),
                          Records = kterl_result:get_records(Res),
                          lists:sort(KV) == lists:sort(Records)
                  end,
    [ ?_assertEqual(ok, ok(kterl:set(C, K64B, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K128B, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K256B, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K1K, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K2K, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K64K, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K256K, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K512K, Value64K)))
      ,?_assertEqual(ok, ok(kterl:set(C, K1M, Value64K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K64B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K128B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K256B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K1K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K2K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K64K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K256K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K512K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K1M)))
      ,?_assertMatch(L when L == length(KV), num(kterl:set_bulk(C,KV)))
      ,?_assertEqual(true, CompareBulk())
      ,?_assertEqual(Value64K, val(kterl:seize(C, K64B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K128B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K256B)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K1K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K2K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K64K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K256K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K512K)))
      ,?_assertEqual(Value64K, val(kterl:seize(C, K1M)))
      ,?_assertEqual(ok, ok(kterl:vacuum(C)))
    ].

set_get_big_values(C) ->
    K2 = big_binary(1 bsl 8),
    K4 = big_binary(1 bsl 9),
    V2M = big_binary(1 bsl 21),
    V4M = big_binary(1 bsl 22),
    [?_assertEqual(ok, ok(kterl:set(C, K2, V2M)))
     ,?_assertEqual(ok, ok(kterl:set(C, K4, V4M)))
     ,?_assertEqual(V2M, val(kterl:seize(C, K2)))
     ,?_assertEqual(V4M, val(kterl:seize(C, K4)))
     ,?_assertEqual(2, num(kterl:set_bulk(C, [{K2,V2M},{K4,V4M}])))
     ,?_assertEqual(V2M, val(kterl:get(C, K2)))
     ,?_assertEqual(V4M, val(kterl:get(C, K4)))
     ,?_assertEqual(ok, ok(kterl:remove(C, K2)))
     ,?_assertEqual(ok, ok(kterl:remove(C, K4)))
     ,?_assertEqual(ok, ok(kterl:vacuum(C)))
    ].
    
exptime(C) ->
    S = fun(N) ->
                calendar:gregorian_seconds_to_datetime(
                  N + calendar:datetime_to_gregorian_seconds(
                        calendar:local_time()))
        end,
    DT1 = S(1),
    DT3 = S(3),
    [ ?_assertEqual(ok, ok(kterl:set(C, <<"one_s">>, <<>>, [{xt, 1}])))
      ,?_assertEqual(ok, ok(kterl:set(C, "one_dt", <<>>, [{xt, DT1}])))
      ,?_assertEqual(ok, ok(kterl:set(C, "three_s", "", [{xt, 3}])))
      ,?_assertEqual(ok, ok(kterl:set(C, <<"three_dt">>, <<>>, [{xt, DT3}])))
      ,?_assertEqual(<<>>, val(kterl:get(C, <<"one_s">>)))
      ,?_assertEqual(<<>>, val(kterl:get(C, <<"one_dt">>)))
      ,?_assertEqual(ok, timer:sleep(timer:seconds(2)))
      ,?_assertEqual({error, no_record}, kterl:get(C, "one_s"))
      ,?_assertEqual({error, no_record}, kterl:get(C, "one_dt"))
      ,?_assertEqual(<<>>, val(kterl:get(C, "three_s")))
      ,?_assertEqual(<<>>, val(kterl:get(C, "three_dt")))
      ,?_assertEqual(ok, timer:sleep(timer:seconds(2)))
      ,?_assertEqual({error, no_record}, kterl:get(C, "three_s"))
      ,?_assertEqual({error, no_record}, kterl:get(C, "three_dt"))
    ].

vacuum(C) ->                    
    [?_assertEqual(ok, ok(kterl:vacuum(C)))].

concurrent_test_() ->
    {ok,C} = kterl:start_link(),
    %kterl:configure(C,[{wire_dump,true}]),
    %Nprocs = 32000,
    Nprocs = 1000,
    spawn_link(fun() -> dump_monitor(C) end),
    [?_assertEqual(ok, kterl:clear(C))
     ,{timeout, 600, ?_assertEqual(ok, concurrent_1(C, Nprocs))}
    ].
    

%%% ===========================================================================

dump_monitor(C) ->
    I = [erlang:process_info(C, W) || 
            W <- [message_queue_len, total_heap_size, heap_size, stack_size, 
                  reductions, garbage_collection]],
    io:format(user,"~nMonitor ~p ~p =====~n~p~n",[C, calendar:local_time(), I]),
    io:format(user,"~p~n",[kterl:local_stats(C)]),
    ok = timer:sleep(timer:seconds(2)),
    dump_monitor(C).

concurrent_1(C, Nprocs) ->
    Pid = self(),
    ?debugFmt("standby. running concurrent bulk set/get test.",[]),
    Pids = [spawn_link(fun() -> ctproc(C, Pid) end) || _ <- lists:seq(1,Nprocs)],
    {ok,N} = pcollect(Pids,0),
    ?debugFmt("~p recs ~p procs",[N,Nprocs]),
    ok.

pcollect([],N) -> {ok,N};
pcollect([Pid|T],N) -> 
    receive 
        {Pid, {ok,Added}} ->
            pcollect(T,N+Added);
        {Pid, Err} ->
            Err
    end.


ctproc(C, Ppid) ->
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    Nsleep = random:uniform(5), %%30
    Nbulk = random:uniform(100), %%250
    io:format(user,"~p nrecs=~p sleep=~p~n",[self(), Nbulk, Nsleep]),
    timer:sleep(timer:seconds(Nsleep)),
    Mypid = pid_to_list(self()),
    Recs = [{list_to_binary(Mypid ++ integer_to_list(N)), 
             list_to_binary(integer_to_list(N))} || N <- lists:seq(1,Nbulk)],
    Keys = [Key || {Key,_} <- Recs],

    case random:uniform(2) of
        1 ->
            {ok,R1} = kterl:bin_set_bulk(C, Recs),
            ?assertEqual(Nbulk, R1);
        2 ->
            {ok,R1} = kterl:set_bulk(C, Recs),
            ?assertEqual(Nbulk, kterl_result:get_num(R1))
    end,

    timer:sleep(timer:seconds(Nsleep)),

    case random:uniform(2) of
        1 ->
            {ok,R2} = kterl:get_bulk(C, Keys),
            ?assertEqual(lists:sort(Recs), lists:sort(kterl_result:get_records(R2)));
        2 ->
            {ok,R2} = kterl:bin_get_bulk(C, Keys),
            ?assertEqual(lists:sort(Recs), 
                         lists:sort([{K,V} || #kt_bin_rec{key=K,val=V} <- R2]))
    end,

    Cursor = kterl:cursor(C),
    ok = kterl:cur_jump(Cursor, [{key,hd(Keys)}]),

    case random:uniform(200) of
        1 ->
            io:format(user, "running cursor sweep ~p~n",[self()]),
            timer:sleep(timer:seconds(Nsleep)),
            CurCnt = cursor_sweep(Cursor, Recs),
            io:format(user, "~p CurCnt ~p (~p)~n",[self(),CurCnt,Nbulk]),
            kterl:remove_bulk(C,Keys);
        _ ->
            kterl:bin_remove_bulk(C,Keys),
            ok
    end,
    ok = kterl:release_cursor(Cursor),

    Ppid ! {self(), {ok, Nbulk}}.


bulk_kv_string_recs(NRecs) ->
    [{"bulk_s_key_" ++ integer_to_list(N), "bulk_s_val_" ++ integer_to_list(N)} ||
        N <- lists:seq(1, NRecs)].

bulk_kv_bin_recs(NRecs) ->
    [{list_to_binary("bulk_b_key_" ++ integer_to_list(N)), 
      list_to_binary("bulk_b_val_" ++ integer_to_list(N))} || 
        N <- lists:seq(1, NRecs)].

cursor_sweep(Cursor, Recs) ->
    cursor_sweep(Cursor,Recs,undefined).

cursor_sweep(Cursor, Recs, StepFun) ->
    cursor_sweep(Cursor, Recs, StepFun, 0).
cursor_sweep(Cursor, Recs, StepFun, N) ->
    case kterl:cur_get(Cursor,true) of
        {ok, Res} ->
            K = kterl_result:get_key(Res),
            V = kterl_result:get_value(Res),
            KV = {K,V},

            case is_function(StepFun) of
                true -> StepFun(Cursor);
                false -> ok
            end,

            case lists:member(KV, Recs) of
                true ->
                    cursor_sweep(Cursor,Recs,StepFun,N+1);
                false ->
                    cursor_sweep(Cursor,Recs,StepFun,N)
            end;
        {error, invalid_cursor} ->
            {ok, N}
    end.

big_binary(Size) ->            
    big_binary(0, Size, random:uniform(16#FF), []).
big_binary(Size, Size, _, Acc) ->
    list_to_binary(Acc);
big_binary(N, Size, LC, Acc) ->
    big_binary(N + 1, Size, LC + 1, [LC rem 16#FF | Acc]).
