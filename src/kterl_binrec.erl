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

-module(kterl_binrec).

-include("kterl.hrl").

-export([new/2, new/3, new/4, get_key/1, get_value/1, get_db/1,
         get_exptime/1, get_exptime_datetime/1]).

new(Key, Value) ->
    new(Key, Value, 0, 0).
new(Key, Value, Database) ->
    new(Key, Value, Database, 0).
new(Key, Value, Database, XT0) ->
    XT = case XT0 of
             N when is_integer(N) -> N;
             _ -> kterl_util:future_seconds(XT0)
         end,
    #kt_bin_rec{key = kterl_util:to_bin(Key), 
                val = kterl_util:to_bin(Value), 
                dbidx = Database, 
                xt = XT}.



-spec get_key(#kt_bin_rec{}) -> binary().
    
get_key(#kt_bin_rec{key = Key}) -> Key.

-spec get_value(#kt_bin_rec{}) -> binary().

get_value(#kt_bin_rec{val = Value}) -> Value.

-spec get_db(#kt_bin_rec{}) -> non_neg_integer().

get_db(#kt_bin_rec{dbidx = DB}) -> DB.

-spec get_exptime(#kt_bin_rec{}) -> undefined | non_neg_integer().

get_exptime(#kt_bin_rec{xt = XT}) -> XT.

-spec get_exptime_datetime(#kt_bin_rec{}) -> undefined | calendar:datetime().

get_exptime_datetime(#kt_bin_rec{xt = XT}) ->
    case XT of
        undefined -> undefined;
        N when is_integer(N) -> kterl_util:unix_epoch_to_local_datetime(N)
    end.
