# kterl

A [Kyoto Tycoon](http://fallabs.com/kyototycoon) client for Erlang.

Features:

 * 100% feature support of Kyoto Tycoon's HTTP and binary protocols.
 * Proven non-blocking protocol handler, derived from [eredis](https://github.com/wooga/eredis).
 * Easy to use and fully documented, with plenty of examples.
 * Clean, consistent, idiomatic interface.
 * OTP compliant.

## Examples

Retrieve and build kterl:

        git clone git://github.com/rcb/kterl.git
        cd kterl
        make && make docs
        erl -pa ebin/

Download and install kyoto tycoon onto your machine, and start it with
a cache tree (in-memory b+tree) database:

        ktserver %
	
With ktserver now running on localhost, it's easy to start experimenting:

        Eshell V5.9.1  (abort with ^G)
        1> {ok, Pid} = kterl:start_link().

        =INFO REPORT==== 3-Apr-2012::14:08:25 ===
        Attempting connection to 127.0.0.1:1978

        =INFO REPORT==== 3-Apr-2012::14:08:25 ===
        Connected to Kyoto Tycoon server at 127.0.0.1:1978
        {ok,<0.35.0>}
        2>

### [Set | add | replace | append | remove] records from Kyoto Server

Kyoto Tycoon supports the standard set of key value set/add/replace/append/remove operations. The application can pass to kterl lists or binaries as key/values.

        1> ok = kterl:add(Pid, <<"hello">>, <<"world">>).
        ok
        2> {ok, Res} = kterl:get(Pid, "hello").
        {ok, ...}
        3> kterl_result:get_value(Res).
        <<"world">>
        4> ok = kterl:replace(Pid, "hello", <<"github">>).
        ok
        5> kterl_result:get_value(kterl:get(Pid, "hello")).
        <<"github">>
        6> ok = kterl:append(Pid, <<"hello">>, <<"...">>).
        ok
        7> kterl_result:get_value(kterl:get(Pid, "hello")).
        <<"github...">>
        8> ok = kterl:remove(Pid, "hello").
        ok
        9> kterl:get(Pid, "hello").
        {error,no_record}
        10> e(7).
        {error,no_record}

Kyoto Tycoon supports atomic record compare-and-swap and compare-and-remove operations. See the documentation for <code>kterl:cas/3</code> and <code>kterl:cas/4</code> for further information.

### Automatic value conversions

As a convenience, kterl automatically converts integer, float, or atom values to binary before storing the record. 

        kterl:add(Pid, "int_key", 12345).
        kterl_result:get_value(kterl:get(Pid, "int_key")).
        <<"12345">>

        kterl:add(Pid, "float_key", 12345.123).
        kterl_result:get_value(kterl:get(Pid, "float_key")).
        <<"12345.123000">>

        kterl:add(Pid, "atom_key", hello_world).
        kterl_result:get_value(kterl:get(Pid, "atom_key")).
        <<"hello_world">>

        kterl:append(Pid, "atom_key", 123456).
        kterl_result:get_value(kterl:get(Pid, "atom_key")).
        <<"hello_world123456">>

        kterl:add(Pid, "erlang_term", term_to_binary([hello,{1,2,3,4},<<"testing">>])).
        binary_to_term(kterl_result:get_value(kterl:get(Pid, "erlang_term"))).
        [hello,{1,2,3,4},<<"testing">>]

### Bulk record operations, using the HTTP or binary protocol

Kyoto Tycoon supports bulk record set/get/remove operations through HTTP or a binary protocol. kterl supports both access methods. In this snippet, 1000 key/value pairs are added, and kterl_result:get_num() is then called on the set_bulk result to extract the number of records added by the server:

        11> Recs = [{"key_" ++ integer_to_list(N), N} || N <- lists:seq(1,1000)].
        ...
        12> {ok, Res1} = kterl:set_bulk(Pid, Recs).
        {ok, ...}
        13> kterl_result:get_num(Res1).
        1000

Here's the binary protocol bulk set call:

        14> kterl:bin_set_bulk(Pid, Recs).
        {ok, 1000}

Bulk retrieval is easy using the HTTP protocol...

        15> Keys = [Key || {Key, _} <- Recs].
        ...
        16> {ok, Res2} = kterl:get_bulk(Pid, Keys).
        {ok,...}
        17> kterl_result:get_num(Res2).
        1000
        18> length(kterl_result:get_records(Res2)).
        1000
        19> hd(kterl_result:get_records(Res2)).
        {<<"key_1">>,<<"1">>}

... or the binary protocol:

        20> {ok, BinRes} = kterl:bin_get_bulk(Pid, Keys).
        {ok, ...}
        21> length(BinRes).
        1000
        22> Fbr = hd(BinRes).
        {kt_bin_rec,0,1099511627775,<<"key_1000">>,<<"1000">>}
        23> kterl_binrec:get_key(Fbr).
        <<"key_1000">>
        24> kterl_binrec:get_value(Fbr).
        <<"1000">>
        25> kterl:bin_remove_bulk(Pid, Keys).
        {ok, 1000}

As a convenience, the application can also pass an erlang dict() to the bulk set calls:
        
        D = dict:from_list([{"key1","val1"}, {"key2","val2"}]).
        kterl:set_bulk(Pid, D).
        kterl:bin_set_bulk(Pid, D).

### Bulk record operations - Binary vs HTTP. Which is better?

The HTTP protocol has a richer feature set than the binary protocol. It allows the application to specify target databases with a filename or a numeric identifier, and can also interact with Kyoto Tycoon's signaling mechanisms. The primary reason to use the binary calls is performance, especially for large bulk get operations:

        D = dict:from_list([{"k_" ++ integer_to_list(N), N} || N <- lists:seq(1,10000)]).
        {ok, 10000} = kterl:bin_set_bulk(Pid, D).
        timer:tc(kterl,get_bulk,[Pid, dict:fetch_keys(D)]).
        {90587, ...}
        timer:tc(kterl,bin_get_bulk,[Pid, dict:fetch_keys(D)]).
        {35183, ...}


### Automatic Record expiration:

Kyoto Tycoon supports automatic record expiration:

        kterl:add(Pid, "exprec", "5..4..3..", [{xt, 5}]).
        kterl_result:get_value(kterl:get(Pid, "exprec")).
        <<"5..4..3..">>
        timer:sleep(timer:seconds(5)).
        kterl:get(Pid,"exprec").
        {error,no_record}

As a convenience, kterl converts calendar:datetime() types when setting an expiration time:

        calendar:gregorian_seconds_to_datetime(
               20 + calendar:datetime_to_gregorian_seconds(calendar:local_time())). 
        {{2012,4,3},{22,30,35}}

        XT = calendar:gregorian_seconds_to_datetime(
               20 + calendar:datetime_to_gregorian_seconds(calendar:local_time())).
        kterl:add(Pid, "exprec", "20..19..18..", [{xt, XT}]).
        kterl_result:get_value(kterl:get(Pid,"exprec")).
        <<"20..19..18..">>
        timer:sleep(timer:seconds(20)).
        kterl_result:get_value(kterl:get(Pid,"exprec")).
        {error,no_record}

Many functions in kterl allow the application to set a record's expiration time. Please check the documentation for additional information.

### Counters

Kyoto Tycoon records can contain a value representing a counter:

        1> kterl:get(Pid, "inc_test").
        {error,no_record}
        2> {ok,Res} = kterl:increment(Pid, "inc_test", 1).
        ...
        3> kterl_result:get_num(Res).
        1
        4> kterl_result:get_num(kterl:increment(Pid, "inc_test", -2)).
        -1

### Regex matching

        1> {ok, Res} = kterl:match_regex(Pid, "(^c..$|^(a|z).$)").
        {ok, ...}
        2> kterl_result:get_keys(Res).
        [<<"ad">>,<<"ah">>,<<"am">>,<<"an">>,<<"as">>,<<"at">>,
         <<"ax">>,<<"ay">>,<<"cab">>,<<"cad">>,<<"cam">>,<<"can">>,
         <<"cap">>,<<"car">>,<<"cat">>,<<"caw">>,<<"chi">>,<<"cob">>,
         <<"cod">>,<<"cog">>,<<"con">>,<<"coo">>,<<"cop">>,<<"cot">>,
         <<"cow">>,<<"cox">>,<<"coy">>,<<"cry">>,<<...>>|...]

### Cursors

Kyoto Tycoon supports database cursors. With these, an application can traverse, access, and mutate records.

        1> {ok, Pid} = kterl:start_link().
        ...
        2> Cursor = kterl:cursor(Pid).
        {kterl_cursor,1,<0.35.0>}
        3> kterl:cur_jump(Cursor, [{key, "key_name"}]).
        ok
        4> {ok, Cres} = kterl:cur_get(Cursor).
        ...
        5> kterl_result:get_key(Cres).
        <<"key_name">>
        6> kterl_result:get_value(Cres).
        <<"record_value">>

Iterating over records in a database is simple. In this example, the print_record function instructs the Kyoto Tycoon server to retrieve a record's key and value, and then automatically step the cursor to an adjacent record.

        print_database(Cursor) ->
            case kterl:cur_get(Cursor, true) of
                {ok, Res} ->
                    Key = kterl_result:get_key(Res),
                    Value = kterl_result:get_value(Res),
                    io:format("~p ~p~n",[Key, Value]),
                    print_database(Cursor);
                {error, invalid_cursor} ->
                    ok
            end.

        print_forward(Cursor) ->
            ok = kterl:cur_jump(Cursor),
            print_database(Cursor).

        print_reverse(Cursor) ->
            ok = kterl:cur_jump_back(Cursor),
            print_database(Cursor).

There are many cursor calls available: seize (get and remove), setting a record's value, removing records, and so on. Please refer to the documentation.

### Much more...

Kyoto Tycoon also supports server-side scripting, client-side signaling, and much more. Please review the documentation for additional information.
