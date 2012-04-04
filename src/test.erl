%% @private
-module(test).

-include("kterl.hrl").

-export([void/0,
         clear/0,
         report/0,
         status/0,
         sr/2,
         srm/0,
         srm/2,
         srmb/2,
         echo/0,
         echo/1,
         cursor/0,
         sb/0,
         gb/0,
         rb/0,
         bsb/0,
         bgb/0,
         brb/0,
         vacuum/0,
         seize/0,
         seize/1,
         set/2,
         add/2,
         replace/2,
         append/2,
         remove/1,
         set_bulk/1, 
         set_bulk/2,
         get_bulk/1,
         get_bulk/2,
         bulk_op_n/2,
         remove_bulk/1,
         remove_bulk/2,
         bin_get_bulk/1,
         bin_set_bulk/1,
         get/1,
         get_wait/3,
         set_sig/4,
         set_wait/4,
         increment/2,
         increment_double/2,
         synchronize/0,
         match_prefix/1,
         match_regex/1,
         start_link/0]).
         
-export([load_dictionary/0]).         

start_link() ->
    {ok,Pid} = kterl:start_link("127.0.0.1",1978,5000),
    Pid.

test1d(F) when is_function(F) ->
    application:start(sasl),
    {ok,C} = kterl:start_link(),
    ok = kterl:configure(C, [{wire_dump, true}]),
    R = F(C),
    ok = kterl:stop(C),
    R.

test1(F) when is_function(F) ->
    application:start(sasl),
    {ok,C} = kterl:start_link(),
    R = F(C),
    ok = kterl:stop(C),
    R.

prout(M,F,A) ->
    try erlang:apply(M,F,A) of
        R ->
            io:format("~n~p:~p(~p) -> ~p~n",[M,F,A,R]),
            R
    catch throw:{http_result,{ResultCode, Msg}} ->
            io:format("HTTP RESULT ~p : ~p~n",[ResultCode, Msg])
    end.


void() ->
    test1d(fun(C) -> prout(kterl,void,[C]) end).

clear() ->
    test1d(fun(C) -> prout(kterl,clear,[C]) end).

report() ->
    test1d(fun(C) -> prout(kterl,report,[C]) end).

status() ->
    test1d(fun(C) -> prout(kterl,status,[C]) end).

kv(KV) ->
    dict:from_list(KV)
    %KV
    .

echo() ->
    test1d(
      fun(C) ->
              EL = [{"key1","val1"},{<<"key2">>,<<"val2">>}, 
                    {<<"key3">>,"val3"},{"key4",<<"val4">>}],
              prout(kterl,echo,[C, kv(EL)])
      end).

echo(KVL) ->
    test1d(fun(C) -> prout(kterl,echo,[C, KVL]) end).
            

cursor() ->
    test1d(
      fun(Client) ->
              Cursor = kterl:cursor(Client),
              lists:foreach(
                fun(Char) ->
                        Key = "key_" ++ [Char],
                        Value = "value->" ++ [Char],
                        R = kterl:set(Client, Key, Value),
                        io:format("~p ~p ~p -> ~p~n",[Char, Key, Value, R])
                end, lists:seq($A, $Z)),
              prout(kterl,cur_jump,[Cursor, "key_A"]),
              prout(kterl,cur_get_key,[Cursor]),
              prout(kterl,cur_get_value,[Cursor]),
              prout(kterl,cur_get,[Cursor, true]),
              prout(kterl,cur_get,[Cursor]),
              prout(kterl,cur_step,[Cursor]),
              prout(kterl,cur_set_value,[Cursor, "overwritten value!"]),
              prout(kterl,cur_get,[Cursor]),
              prout(kterl,release_cursor,[Cursor])
      end).

sr(K,V) ->
    test1d(fun(C) -> kterl:set(C,K,V) end).

srm() ->
    srm("testing", 10).

srm(Pfx,Num) -> 
    timer:start(),
    test1(
      fun(C) ->
              {T,_} = timer:tc(fun() -> srm_n(Num, C, Pfx) end),
              io:format("~nTIME ~p = ~p (~p)~n", [Num, T, T/Num]),
              Stats = prout(kterl,local_stats,[C]),
              io:format("local_stats = ~p~n",[Stats]),
              Stats
      end).

srmb(Pfx,Num) -> 
    timer:start(),
    test1(
      fun(C) ->
              {T,_} = timer:tc(fun() -> srm_nb(Num, C, Pfx) end),
              io:format("~nTIME ~p = ~p (~p)~n", [Num, T, T/Num]),
              Stats = prout(kterl,local_stats,[C]),
              io:format("local_stats = ~p~n",[Stats]),
              Stats
      end).

srm_n(0,_, _) -> ok;
srm_n(N,C,Pfx) ->
    Ext = Pfx ++ "_" ++ integer_to_list(N),
    Key = "K_" ++ Ext,
    Val = "V_" ++ Ext,
    %kterl:set(C, Key, Val, [{database, 0}, {xt, 120}]),
    kterl:set(C,Key,Val),
    srm_n(N - 1, C, Pfx).

srm_nb(0,_, _) -> ok;
srm_nb(N,C,Pfx) ->
    Ext = Pfx ++ "_" ++ integer_to_list(N),
    Key = "K_" ++ Ext,
    Val = "V_" ++ Ext,
    %kterl:set(C, Key, Val, [{database, 0}, {xt, 120}]),
    kterl:bin_set_bulk(C, [{Key, Val}]),
    srm_nb(N - 1, C, Pfx).




sb() ->
    test1d(
      fun(C) ->
              Recs = [{"bulk_key_" ++ [Chr], "bulk_val_" ++ [Chr]} || Chr <- lists:seq($a, $z)],
              prout(kterl,set_bulk,[C, kv(Recs)])
      end).

gb() ->
    test1d(
      fun(C) ->
              Keys = ["bulk_key_" ++ [Chr] || Chr <- lists:seq($a, $z)],
              prout(kterl,get_bulk,[C, Keys])
      end).

rb() ->    
    test1d(
      fun(C) ->
              Keys = ["bulk_key_" ++ [Chr] || Chr <- lists:seq($a, $z)],
              prout(kterl,remove_bulk,[C, Keys, [{atomic, true}]])
      end).

bsb() ->
    test1d(
      fun(C) ->
              % Recs = [{"bin_bulk_key_" ++ [Chr], "bin_bulk_val_" ++ [Chr], 0, 120} || 
              %            Chr <- lists:seq($a, $z)],

              Recs = [#kt_bin_rec{key = "bin_bulk_key_" ++ [Chr],
                                  val = "bin_bulk_val_" ++ [Chr]
                                  %,xt  = {{2012,3,17},{7,5,0}}
                                 }
                      || Chr <- lists:seq($a, $z)],

              prout(kterl,bin_set_bulk,[C, Recs])
      end).

bgb() ->
    %bsb(),
    test1d(
      fun(C) ->
              Keys = ["bin_bulk_key_" ++ [Chr] || Chr <- lists:seq($a, $z)],
              prout(kterl,bin_get_bulk,[C, Keys])
      end).

brb() ->
    %bsb(),
    test1d(
      fun(C) ->
              Keys = ["bin_bulk_key_" ++ [Chr] || Chr <- lists:seq($a, $z)],
              R = prout(kterl,bin_remove_bulk,[C, Keys]),
              %GB = prout(kterl,bin_get_bulk,[C, Keys]),
              %io:format("GB after remove=~p~n",[GB]),
              R
      end).

vacuum() ->
    test1d(fun(C) -> prout(kterl,vacuum,[C]) end).

seize() ->
    test1d(
      fun(C) ->
              K = <<"seize_k">>,
              prout(kterl,add,[C, K, <<"seize_v">>]),
              Sr = prout(kterl,seize,[C, K]),
              Gr = prout(kterl,get,[C, K]),
              io:format("Sr = ~p~nGr = ~p~n",[Sr, Gr])
      end).

seize(K) ->
    test1d(fun(C) -> prout(kterl,seize,[C, K]) end).

set(K,V) ->                   
    test1d(fun(C) -> prout(kterl,set,[C,K,V]) end).

add(K,V) ->
    test1d(fun(C) -> prout(kterl,add,[C, K, V]) end).

replace(K,V) ->
    test1d(fun(C) -> prout(kterl,replace,[C, K, V]) end).

append(K,V) ->
    test1d(fun(C) -> prout(kterl,append,[C, K, V]) end).

remove(K) ->
    test1d(fun(C) -> prout(kterl,remove,[C, K]) end).

set_bulk(KV) ->
    test1d(fun(C) -> prout(kterl,set_bulk,[C, KV]) end).

set_bulk(KV,OptArgs) ->
    test1d(fun(C) -> prout(kterl,set_bulk,[C, KV, OptArgs]) end).

get_bulk(KV) ->
    test1d(fun(C) -> prout(kterl,get_bulk,[C, KV]) end).

get_bulk(KV,OptArgs) ->
    test1d(fun(C) -> prout(kterl,get_bulk,[C, KV, OptArgs]) end).


bulk_op_n(Op, N) when is_atom(Op), is_integer(N) ->
    Bopk = "bop_k_",
    Bopv = "bop_v_",
    KeyF = fun() -> [list_to_binary(Bopk ++ integer_to_list(I)) || 
                        I <- lists:seq(1,N)]
           end,
    KeyValF = fun() -> [{list_to_binary(Bopk ++ integer_to_list(I)),
                         list_to_binary(Bopv ++ integer_to_list(I))} ||
                           I <- lists:seq(1,N)]
              end,

    case Op of
        set ->
            KV = KeyValF(),
            test1(fun(C) -> kterl:set_bulk(C,KV) end);   %%prout(kterl, set_bulk, [C,KV]) end);
        bin_set ->
            KV = KeyValF(),
            test1(fun(C) -> kterl:bin_set_bulk(C,KV) end);

        get ->
            K = KeyF(),
            test1(fun(C) -> kterl:get_bulk(C,K) end);    %%prout(kterl, get_bulk, [C,K]) end);
        bin_get ->
            K = KeyF(),
            test1(fun(C) -> kterl:bin_get_bulk(C,K) end);
        remove ->
            K = KeyF(),
            test1(fun(C) -> kterl:remove_bulk(C,K) end);  %%prout(kterl, remove_bulk, [C,K]) end)
        bin_remove ->
            K = KeyF(),
            test1(fun(C) -> kterl:bin_remove_bulk(C,K) end)
    end.
    

remove_bulk(KV) ->
    test1d(fun(C) -> prout(kterl,remove_bulk,[C,KV]) end).

remove_bulk(KV, OptArgs) ->
    test1d(fun(C) -> prout(kterl,remove_bulk,[C,KV,OptArgs]) end).

bin_get_bulk(Keys) ->
    test1d(fun(C) -> prout(kterl,bin_get_bulk,[C,Keys]) end).

bin_set_bulk(KV) ->
    test1d(fun(C) -> prout(kterl,bin_set_bulk,[C,KV]) end).
                   
get(K) ->
    test1d(fun(C) -> prout(kterl,get,[C,K]) end).

get_wait(K, Wait, Waittime) ->
    test1d(fun(C) -> prout(kterl,get,[C,K,[{wait, Wait}, {waittime, Waittime}]]) end).

set_sig(K, V, Signal, Signalbroad) ->
    test1d(fun(C) -> prout(kterl,set,[C,K,V,[{signal, Signal}, 
                                             {signalbroad, Signalbroad}]]) end).

set_wait(K,V,Wait,Waittime) ->
    test1d(fun(C) -> prout(kterl,set,[C,K,V,[{wait,Wait},{waittime,Waittime}]]) end).
                   

increment(K,V) ->
    test1d(fun(C) -> prout(kterl,increment,[C, K, V]) end).

increment_double(K,V) ->
    test1d(fun(C) -> prout(kterl, increment_double, [C, K, V]) end).
                   
synchronize() ->
    test1d(fun(C) -> prout(kterl,synchronize,[C]) end).

match_prefix(Pfx) ->
    test1d(fun(C) -> prout(kterl,match_prefix,[C,Pfx]) end).

match_regex(Regex) ->
    test1d(fun(C) -> prout(kterl,match_regex,[C,Regex]) end).
              

%%% TIME 500000 = 53391906 (106.783812) (pure binary implementation)
%%% TIME 100000 = 11821824 (118.21824)  (iovec passed into gen server w/ db & xt params)
%%% TIME 500000 = 61432230 (122.86446)  ("")
%%% TIME 500000 = 52610252 (105.220504) (pure binary w/o xt or database parameters)
%%% TIME 500000 = 54112029 (108.224058) (iovec w/o xt or database parameters)
%%% ubuntu:
%%% TIME 500000 = 120737657 (241.475314) (iovec)
%%% TIME 500000 = 119494819 (238.989638) (binary)
%%% TIME 500000 = 116753569 (233.507138) (with #ktreq_)
%%% bin_set_bulk (srmb call)
%%% ubuntu:
%%% TIME 500000 = 81285359 (162.570718)

%% test1() ->
%%     Res = kterl:get("key"),
%%     case 

load_dictionary() ->
    {ok, Pid} = kterl:start_link(),
    {ok, B} = file:read_file("/usr/share/dict/words"),
    {ok, Res} = kterl:set_bulk(
                    Pid, [{Word,""} || 
                            Word <- binary:split(B,<<"\n">>,[global,trim])]),
    kterl_result:get_num(Res),
    kterl:stop(Pid).
    


