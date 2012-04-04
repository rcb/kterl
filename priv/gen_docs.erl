#!/usr/bin/env escript
%% -*- erlang -*-

%% this script uses %% __ret__ comments from kterl source code to generate
%% call result documentation for edoc files.

main([]) ->
    io:format("usage: edoc~n");
main(["edoc"]) ->
    edoc();
main(["skel"]) ->
    erlang:exit("are you sure?"),
    mkskel().

edoc() ->
    {ok,Skelbin} = file:read_file(skel_filename()),
    Skel = [binary_to_list(Line) || Line <- split_file(Skelbin)],
    Rets = get_rets(),
    Edocs = edoc(Skel, Rets, []),
    ok = write_edocs(Edocs).

write_edocs([]) -> ok;
write_edocs([{Filename, Funcname, Funcdoc} | T]) -> 
    ok = write_edoc(Filename, Funcname, Funcdoc),
    write_edocs(T).

write_edoc(Edocfile, Funcname, Funcdoc) ->
    Filename = edoc_filename(Edocfile),
    io:format("~s~s~n",[string:left(str(Funcname),20,$.), string:right(Filename,40,$.)]),
    {ok,F} = file:open(Filename, [write]),
    ok = file:write(F, ["%% Notice: This file is auto-generated from '", skel_filename(), "'\n"]),
    ok = write_funcdoc(Funcdoc, F),
    file:close(F).

write_funcdoc([],_) -> ok;
write_funcdoc([Line|T], F) -> 
    ok = file:write(F, [Line, "\n"]),
    write_funcdoc(T,F).

edoc([], _, Acc) -> Acc;
edoc(Buf, Rets, Acc) -> 
    {ok, Filename, Funcdoc_in, Newbuf} = edoc_get_funcdoc(Buf),
    Funcname = file_to_func(Filename),
    Retsyms = get_retsyms(Rets, Funcname),
    Funcdoc = add_retsyms(Funcname, Funcdoc_in, Retsyms),
    edoc(Newbuf, Rets, [{Filename, Funcname, Funcdoc} | Acc]).

add_retsyms(Funcname, Funcdoc, Retsyms) when Funcname =:= undefined; Retsyms =:= undefined -> Funcdoc;
add_retsyms(Funcname, Funcdoc, Retsyms) ->
    Retdoc = gen_retsyms_doc(Funcname, Retsyms),
    insert_retsyms_doc(Funcdoc, Retdoc).

gen_retsyms_doc(Funcname, ["ok"]) ->
    ["<p/>On success " ++ Funcname ++ " returns <code>'ok'</code>.<p/>"];
gen_retsyms_doc(Funcname, Retsyms) ->
    ["<p/>On success " ++ Funcname ++ " will return <code>{ok, Result}</code>. The <code>Result</code> can then be passed to the "
     "following function(s) to access:<p/>```",
     [ string:left("kterl_result:" ++ RS ++ "(Result).", 42) ++ "% " ++ retsym_desc(RS) ++ "\n" || RS <- Retsyms],"'''"].

insert_retsyms_doc(Funcdoc, Retdoc) ->
    {H,T} = find_insert_point(Funcdoc,[]),
    H ++ Retdoc ++ T.

find_insert_point([], Acc) -> {lists:reverse(Acc), []};
find_insert_point(T = ["@see " ++ _ | _], Acc) -> {lists:reverse(Acc), T};
find_insert_point([H|T], Acc) -> find_insert_point(T, [H|Acc]).

retsym_desc(RS) ->
    case RS of 
        "get_value" ->   "The record's value.";
        "get_key"   ->   "The record's key.";
        "get_keys"  ->   "All matching keys.";
        "get_records" -> "All records.";
        "get_exptime" -> "The record's expiration time (if set.)";
        "get_signaled_count" -> "The number of clients signaled (if 'signal' was passed to the call.)";
        "get_num" ->     "The result from an increment call, or a record count."
    end.
            
    

edoc_get_funcdoc(Buf) -> edoc_get_funcdoc(Buf, "", "").
edoc_get_funcdoc([], Funcname, Funcbuf) ->
    {ok, Funcname, lists:reverse(Funcbuf), []};
edoc_get_funcdoc(["%% __endfile__" | T], Funcname, Funcbuf) when Funcname /= ""; Funcbuf /= "" ->
    {ok, Funcname, lists:reverse(Funcbuf), T};
edoc_get_funcdoc(["%% __file__ " ++ Funcname | T], "", "") -> 
    edoc_get_funcdoc(T, Funcname, "");
edoc_get_funcdoc([Line|T], Funcname, Funcbuf) when Funcname /= "" -> 
    edoc_get_funcdoc(T, Funcname, [Line | Funcbuf]);
edoc_get_funcdoc([_|T], "", "") -> 
    edoc_get_funcdoc(T, "", "").

%% "function_name_3.edoc" -> "function_name/3"
%% "intro.edoc" -> undefined
file_to_func(Filename) -> 
    I = string:str(Filename, ".edoc"),
    case string:substr(Filename, I-2,1) of
        "_" ->
            Func = string:substr(Filename, 1, I-3),
            Arity = string:substr(Filename, I-1, 1),
            Func ++ "/" ++ Arity;
        _ ->
            undefined
    end.

%% [ ["void/1", "ok"], ["echo/2", "get_records", "get_num"], ...]
get_retsyms([], _) -> undefined;
get_retsyms([[Funcname|Rets]|_], Funcname) -> Rets;
get_retsyms([_|T], Funcname) -> get_retsyms(T, Funcname).

mkskel() ->
    {ok,Edocs} = file:list_dir(edoc_filename("")),
    {ok,Skelf} = file:open(skel_filename(), [write, raw]),
    ok = mkskel(Edocs, Skelf),
    ok = file:close(Skelf).

mkskel([],_) -> ok;
mkskel([Edoc|T], Skelf) -> 
    case file:read_file(edoc_filename(Edoc)) of
        {ok,B} ->
            io:format("~p -> ~p~n",[Edoc, byte_size(B)]),
            ok = file:write(Skelf, ["%% __file__ ", Edoc, "\n", B, "\n%% __endfile__\n"]);
        {error, eisdir} ->
            ok
    end,
    mkskel(T, Skelf).
    

get_rets() ->
    {ok,B} = file:read_file(filename:join(["src","kterl.erl"])),
    [string:tokens(L," \t") || L <- [binary_to_list(L) || <<"%% __ret__ ", L/binary>> <- split_file(B)]].

                    
split_file(B) when is_binary(B) ->
    binary:split(B, <<"\n">>, [global, trim]).                    
                
skel_filename() ->
    filename:join(["doc","skel","func_docs.skel"]).

edoc_filename(Filename) ->
    filename:join(["doc","edoc",Filename]).

str(S) when is_list(S) -> S;
str(_) ->  "".
    
