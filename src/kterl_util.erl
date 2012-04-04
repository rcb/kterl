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
%% miscellaneous utility functions for kterl

-module(kterl_util).
-author('rbeer@beerman.org').

-include("kterl.hrl").
-include("ktinternal.hrl").

-export([to_bin/1, to_int/1, decode/2, url_decode/1, base64_decode/1,
         bin_split_kv/1, bin_split_decode_kv/2, future_seconds/1,
         unix_epoch_to_datetime/1, unix_epoch_to_local_datetime/1,
         datetime_to_unix_epoch/1, localtime_epoch/0,
         localtime_epoch_diff_secs/1]).

%% @doc force a string to binary
-spec to_bin(string()) -> binary()
             ; (binary()) -> binary().
to_bin(L) when is_list(L) -> unicode:characters_to_binary(L);
to_bin(B) when is_binary(B) -> B.

-spec to_int(string() | binary() | integer()) -> integer().
to_int(L) when is_list(L) -> list_to_integer(L);
to_int(B) when is_binary(B) -> list_to_integer(binary_to_list(B));
to_int(I) when is_integer(I) -> I.

-spec decode(url | base64 | undefined, SB) ->  SB
      when SB :: string() | binary().

decode(url, V) -> url_decode(V);
decode(base64, V) -> base64_decode(V);
decode(_, V) -> V.

-spec url_decode(kterl:kt_str()) -> kterl:kt_str().

url_decode(L) when is_list(L) -> url_decode_l(L);
url_decode(B) when is_binary(B) -> url_decode_b(B,<<>>).

-spec url_decode_b(binary(), binary()) -> binary().

url_decode_b(<<$%,H1,H2,Rest/binary>>, Acc) ->
    url_decode_b(Rest, <<Acc/binary, (hex2dec(H1) * 16 + hex2dec(H2))>>);
url_decode_b(<<H,Rest/binary>>, Acc) ->
    url_decode_b(Rest, <<Acc/binary, H>>);
url_decode_b(<<>>, Acc) ->
    Acc.

-spec url_decode_l(string()) -> string().

%%% ripped from http_uri:decode()
url_decode_l([$%,H1,H2|T]) ->
    [hex2dec(H1)*16+hex2dec(H2)|url_decode_l(T)];
url_decode_l([H|T]) ->
    [H|url_decode_l(T)];
url_decode_l([]) ->
    [].

-spec hex2dec(char()) -> char().

hex2dec(X) when X >= $0, X =< $9 -> X-$0;
hex2dec(X) when X >= $A, X =< $F -> X-$A+10;
hex2dec(X) when X >= $a, X =< $f -> X-$a+10.

-spec base64_decode(string()| binary()) -> binary().
    
base64_decode(V) -> base64:decode(V).

%% Take a binary of the form:
%% <<"key1\tval1\nkey2\tval2\n">> ...
%% and convert to [{<<"key1">>, <<"val1">>}, {<<"key2">>,<<"val2">>}, ...]
-spec bin_split_kv(Bin :: binary()) -> kterl:kt_bin_kv_list().

bin_split_kv(Bin) ->
    bin_split_decode_kv(Bin, undefined).

%% @doc
%% Performs bin_split_kv and also optionally decodes the k,v pairs
-spec bin_split_decode_kv(Bin::binary(), EncType::url | base64 | undefined) ->
                                 kterl:kt_bin_kv_list().

bin_split_decode_kv(Bin, EncType) ->
    bin_split_decode_kv_1(Bin, EncType).

%% =======================================================================
%% original version --
%% ~10 seconds for 20 meg body w/ 750k k,v pairs
%% 
%% profiling reveals a significant amount of time on large bulk
%% get operations is spent in this function.
%% =======================================================================
     
-spec bin_split_decode_kv_1(Body::binary(), EncType::url | base64 | undefined) ->
    kterl:kt_bin_kv_list().

bin_split_decode_kv_1(Body, EncType) ->
    KV = [binary:split(KV, <<"\t">>) || KV <- binary:split(Body, <<"\n">>, [global])],
    [{decode(EncType, K), decode(EncType, V)} || [K,V] <- KV].

%%% =======================================================================
%%% tried to make it faster, but didn't..
%%% ~18 seconds
%%% =======================================================================

-ifdef(fail).
    
bin_split_decode_kv_2(Bin, EncType) ->
    M = binary:matches(Bin, [<<"\t">>, <<"\n">>]),
    do_bsd_kv(M, 0, Bin, EncType, {undefined, undefined}, []).

do_bsd_kv(L, N, Bin, EncType, {K,V}, Acc) when L == []; N > byte_size(Bin) ->
    Nacc = case {K,V} of
               {undefined, undefined} -> Acc;
               {_, undefined} -> [{decode(EncType,K), <<>>} | Acc];
               {_, _} -> [{decode(EncType,K), decode(EncType,V)} | Acc]
           end,
    lists:reverse(Nacc);
do_bsd_kv([{A, B} | T], N, Bin, EncType, {K, V}, Acc) ->
    S = binary:part(Bin, {N, A-N}),
    {KV, Nacc} = case {K, V} of
                    {undefined, _} ->
                        {{S, undefined}, Acc};
                    {_, undefined} ->
                        {{K, S}, Acc};
                    {_, _} ->
                        {{S, undefined}, 
                         [{decode(EncType, K), decode(EncType, V)} | Acc]}
                end,
    do_bsd_kv(T, A+B, Bin, EncType, KV, Nacc).

-endif.

%%% =======================================================================
%%% another attempt...
%%% ~19 seconds
%%% =======================================================================

-ifdef(fail).

bin_split_decode_kv_3(Bin, EncType) ->
    L = binary:split(Bin, [<<"\t">>, <<"\n">>], [global, trim]),
    do_bsd_kv3(L,EncType).

do_bsd_kv3([] ,_) -> [];
do_bsd_kv3([Key, Val | T], EncType) ->
    [{decode(EncType, Key), decode(EncType, Val)} | do_bsd_kv3(T, EncType)].

-endif.

%%% =======================================================================
%%% =======================================================================

-spec future_seconds(DateTime::calendar:datetime()) -> integer().

future_seconds(DateTime) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Then = calendar:datetime_to_gregorian_seconds(DateTime),
    Then - Now.

% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) = 62167219200
-define(UNIX_EPOCH, 62167219200).
    
-spec unix_epoch_to_local_datetime(Epoch) -> calendar:datetime()
      when Epoch :: non_neg_integer().

unix_epoch_to_local_datetime(Epoch) ->
    calendar:universal_time_to_local_time(
      calendar:gregorian_seconds_to_datetime(Epoch + ?UNIX_EPOCH)).

-spec unix_epoch_to_datetime(Epoch::non_neg_integer()) -> calendar:datetime().

unix_epoch_to_datetime(Epoch) ->
    calendar:gregorian_seconds_to_datetime(Epoch + ?UNIX_EPOCH).
    
-spec datetime_to_unix_epoch(DateTime :: calendar:datetime()) -> non_neg_integer().

datetime_to_unix_epoch(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.

-spec localtime_epoch() -> non_neg_integer().
localtime_epoch() ->
    {M,S,_} = now(),
    M * 1000000 + S.

-spec localtime_epoch_diff_secs(Epoch :: non_neg_integer()) -> integer().
localtime_epoch_diff_secs(Epoch) ->
    Epoch - localtime_epoch().
    

%%% =======================================================================
%%% =======================================================================
    
-ifdef(experimental).
-export([ideal_encoding_method/1]).

%% @doc
%% translated from kyoto tycoon's checkmapenc() function
%% this needs work.
%%
-spec ideal_encoding_method(kterl:kt_bin_kv_list()) -> base64 | url | undefined.

ideal_encoding_method(KVL) ->
    ideal_encoding_method(KVL, 0, 0, false).

ideal_encoding_method([], _, _, false) ->
    undefined;
ideal_encoding_method([], TotBlen, TotUlen, _) ->
    if TotBlen < TotUlen -> base64;
       true -> url
    end;
ideal_encoding_method([{Key, Value} | T], TotBlen, TotUlen, TotBinaryFlag) ->
    {KFlag, KBlen, KUlen} = get_flag_bu_len(Key),
    {VFlag, VBlen, VUlen} = get_flag_bu_len(Value),
    ideal_encoding_method(
      T, TotBlen + KBlen + VBlen, TotUlen + KUlen + VUlen,
      TotBinaryFlag or KFlag or VFlag).

-spec get_flag_bu_len(Binary::binary()) ->
    {BinaryFlag::boolean(), Blen::non_neg_integer(), Ulen::non_neg_integer()}.

get_flag_bu_len(Binary_in) ->
    {Binary, BinFlag} = if byte_size(Binary_in) > 16#FF ->
                                <<B:16#FF/bytes, _/binary>> = Binary_in,
                                {B, true};
                           true ->
                                {Binary_in, false}
                        end,
    Blen = byte_size(Binary) * 6 / 4 + 3,
    {BinaryFlag, Ulen} = get_flag_bu_len(Binary, {BinFlag, 0}),
    io:format("Binary=~p Flag=~p Blen=~p Ulen=~p~n",
              [Binary,BinaryFlag, Blen, Ulen]),
    {BinaryFlag, Blen, Ulen}.

-spec get_flag_bu_len(binary(), T) -> T 
    when T :: {boolean(), non_neg_integer()}.
    
get_flag_bu_len(<<>>, Res) -> Res;
get_flag_bu_len(<<Char:8, Rest/binary>>, {BinaryFlag_in, Ulen}) ->
    BinaryFlag = if Char < $\ orelse Char == 16#7F -> true;
                    true -> false
                 end,
    I = if Char >= $A andalso Char =< $Z -> 1;
           Char >= $a andalso Char =< $z -> 1;
           Char >= $0 andalso Char =< $9 -> 1;
           Char == $_ -> 1;
           Char == $- -> 1;
           Char == $. -> 1;
           Char == $! -> 1;
           Char == $~ -> 1;
           Char == $* -> 1;
           Char == $' -> 1;
           Char == $( -> 1;
           Char == $) -> 1;
           true -> 3
        end,
    get_flag_bu_len(Rest, {BinaryFlag or BinaryFlag_in, Ulen + I}).
-endif.
    
%%% =======================================================================
%%% =======================================================================
