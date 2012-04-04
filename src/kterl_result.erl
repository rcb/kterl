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

-module(kterl_result).
-author('rbeer@beerman.org').

-include("kterl.hrl").

-export([get_key/1, get_val/1, get_value/1, get_exptime/1,
         get_exptime_datetime/1, get_exptime_diff_seconds/1,
         get_signaled_count/1, get_num/1, get_keys/1, get_recs/1,
         get_records/1]).

-type kt_http_result() :: #kt_http_result{}.


-spec get_key(Res :: kt_http_result()) -> Key::binary() | undefined.
get_key(Res) -> getr(Res, #kt_http_result.key).


-spec get_val(Res :: kt_http_result()) -> Value::binary() | undefined.
get_val(Res) -> get_value(Res).


-spec get_value(Res :: kt_http_result()) -> Value::binary() | undefined.
get_value(Res) -> getr(Res, #kt_http_result.value).


-spec get_exptime(Res :: kt_http_result()) -> non_neg_integer() | undefined.
get_exptime(Res) -> getr(Res, #kt_http_result.exptime).


-spec get_exptime_datetime(Res :: kt_http_result()) -> calendar:datetime() 
                                                     | undefined.
get_exptime_datetime(Res) ->
    case getr(Res, #kt_http_result.exptime) of
        Epoch when is_integer(Epoch) ->
            kterl_util:unix_epoch_to_local_datetime(Epoch);
        Other ->
            Other
    end.

-spec get_exptime_diff_seconds(Res :: kt_http_result()) -> integer() 
                                                         | undefined.

get_exptime_diff_seconds(Res) ->
    case getr(Res, #kt_http_result.exptime) of
        Epoch when is_integer(Epoch) ->
            kterl_util:localtime_epoch_diff_secs(Epoch);
        Other ->
            Other
    end.


-spec get_signaled_count(Res :: kt_http_result()) -> non_neg_integer() 
                                                    | undefined.
get_signaled_count(Res) -> getr(Res, #kt_http_result.signaled_count).


-spec get_num(Res :: kt_http_result()) -> number() | undefined.
get_num(Res) -> getr(Res, #kt_http_result.num).


-spec get_keys(Res :: kt_http_result()) -> [Key::binary()] | undefined.
get_keys(Res) -> getr(Res, #kt_http_result.keys).


-spec get_recs(Res :: kt_http_result()) -> [{Key::binary(), Value::binary()}]
                                          | undefined.
get_recs(Res) -> get_records(Res).


-spec get_records(
        Res :: kt_http_result()) -> [{Key::binary(), Value::binary()}]
                                  | undefined.
get_records(Res) -> getr(Res, #kt_http_result.bulk_records).


getr(Res = #kt_http_result{}, N) -> element(N, Res);
getr(Err = {error, _}, _) -> Err;
getr({ok, Res = #kt_http_result{}}, N) -> getr(Res,N);
getr(_,_) -> throw({error, invalid_result_type}).

